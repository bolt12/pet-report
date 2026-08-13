-- | The capture and batch workflows. Capture queues one clean frame per online camera, with
-- no GPU involved. Batch analyses the queued frames and new Frigate events into
-- observations, prunes old proof frames, then synthesises the day's narrative and pushes it.
--
-- A model outage aborts the current step gracefully, leaving frames queued for the next run.
-- Per-item failures are logged and skipped.
module PetReport.Pipeline
  ( capture
  , batch
  , buildDay
  , advanceWatermark
  , proofRetainDays
  , ingestWindow
  , ingestWindowSafe
  , Budget
  , newBudget
  , Stage (..)
  , takeBudget
  , clampToRetention
  , RunOutcome (..)
  , runOutcomeText
  , gcEffectiveWindow
  , garbageCollectNow
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar  (MVar, putMVar, tryTakeMVar)
import           Control.Exception        (SomeException, bracket, handle,
                                           throwIO, try)
import           Control.Concurrent.STM   (readTVarIO)
import           Control.Monad            (foldM, forM_, unless, void, when)
import           Data.Aeson               (object, (.=))
import           Data.Aeson.Text          (encodeToLazyText)
import           Data.IORef               (IORef, atomicModifyIORef',
                                           newIORef, readIORef)
import           Data.List                (sort, transpose)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe, isJust, mapMaybe)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import           Data.Time                (Day, NominalDiffTime, UTCTime,
                                           addDays, addUTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime,
                                           utcTimeToPOSIXSeconds)
import           Data.Time.LocalTime      (localTimeOfDay, todHour)
import           Data.Time.Zones          (TZ, utcToLocalTimeTZ)
import           System.FilePath          ((</>))

import           PetReport.App                 (App (..), appBatchLock,
                                                appRetention)
import           PetReport.Config              (Config (..))
import           PetReport.Domain.Behavior     (Behaviors (..),
                                                accidentSuspected,
                                                injurySuspected)
import           PetReport.Domain.Observation  (FrigateMeta (..),
                                                NewObservation (..),
                                                Observation (..), Origin (..))
import           PetReport.Domain.Perception   (Appearance (..), Perception (..),
                                                Scene (..), SoundKind (..),
                                                isPerson, isSafetySound)
import           PetReport.Domain.PetReport    (BalanceV (..), PetInsights (..),
                                                Spot (..), WellbeingV (..),
                                                insightsFor)
import           PetReport.Domain.Profile      (Overrides, Pet (..), Profile,
                                                activePets, cameras,
                                                enabledCameras, gcWindowDays, pets)
import           PetReport.Domain.Report       (Period (..), Report (..))
import           PetReport.Domain.Stats        (SubjectKey (..),
                                                subjectAppearances)
import           PetReport.Domain.Types        (Camera (..), EventId (..),
                                                Wellbeing (..))
import           PetReport.Domain.Window       (dayKeyText, localDayOf,
                                                localDayWindow, startOfLocalDay)
import qualified PetReport.Effect.Clock        as Clock
import qualified PetReport.Effect.Db           as Db
import qualified PetReport.Effect.Ffmpeg       as Ffmpeg
import           PetReport.Effect.Frigate      (FrigateEvent (..))
import qualified PetReport.Effect.Frigate      as Frigate
import qualified PetReport.Effect.Llm          as Llm
import qualified PetReport.Effect.Ntfy         as Ntfy
import qualified PetReport.Analysis.Narrative  as Narrative
import           PetReport.Pipeline.Queue      (listJpgs, moveToProof, removeQuiet,
                                                tryRead, withTs, writeQueueFrame)
import qualified PetReport.Analysis.Recap      as Recap
import qualified PetReport.Analysis.IdentGuide as IdentGuide
import           PetReport.Util                (boundedLines, tshow)
import           PetReport.Trace               (PipelineEvent (..), SkipReason (..),
                                                Tracer, pipelineTracer, traceWith)
import qualified PetReport.Analysis.Vision     as Vision

-- Tunables (candidates to move into Config later).
proofRetainDays :: Double
proofRetainDays = 30

queueRetainDays :: Double
queueRetainDays = 2

eventCooldownSec :: Double
eventCooldownSec = 600

eventClipFrames :: Int
eventClipFrames = 6

-- | How long a transiently-failed event keeps the ingest watermark held back, so later
-- batches retry it. A snapshot that isn't ready yet, or a model blip, clears within a batch
-- or two. Past this window a persistently unreadable event is abandoned, so it cannot wedge
-- ingestion forever or keep forcing re-analysis of everything after it.
eventRetryWindowSec :: Double
eventRetryWindowSec = 6 * 3600

-- | The wall-clock ceiling on one batch's item processing, and the only bound there is.
--
-- This used to sit alongside an item cap, which was dropped because it bounded the wrong
-- thing: an item is one picture for a queued frame but a snapshot plus 'eventClipFrames'
-- more for an event, so a fixed count was anywhere from a minute of work to a couple of
-- hours. What has to be bounded is how long the single batch worker is held, since every
-- on-demand refresh and day rebuild queues behind it, and that is exactly what this bounds.
--
-- Past the deadline a slow-model run defers its remainder and records an honest "ok"
-- outcome, stopping at the same 'frozen' path a spent allowance always took.
batchDeadlineSecs :: NominalDiffTime
batchDeadlineSecs = 20 * 60

-- | The most events one ingest pass pulls from Frigate in a single request. Frigate returns
-- them OLDEST first, so a request takes the oldest @limit@ after the watermark and 'sweepWindow'
-- asks again from there. Set well above the busiest observed day, so a normal batch drains in
-- one request.
eventFetchLimit :: Int
eventFetchLimit = 500

-- | How recently Frigate must have reported on a camera for 'capture' to leave it alone.
--
-- Samples are the fallback for stretches Frigate says nothing about, so this is the length of
-- silence that counts as a gap worth filling. Too short and the queue fills with frames of
-- rooms the events already cover, crowding out the ingest they are meant to supplement; too
-- long and a genuinely quiet afternoon goes unrecorded.
captureQuietSecs :: Double
captureQuietSecs = 3600

-- | The Frigate labels this app ingests: pet object labels, audio labels, and person labels.
--
-- Shared by the ingest window and by 'capture's quiet check, which must agree. If capture
-- asked about a wider set than ingest stores, a camera would count as busy on an event that
-- never becomes an observation, and the gap it was meant to cover would go unsampled.
--
-- People are ingested as themselves, not as pets. The label only decides what is fetched;
-- who is in the frame is decided by the model, and a person resolves to its own stats
-- bucket, so a visitor cannot land in a pet's meals or rest.
ingestLabels :: Config -> [Text]
ingestLabels cfg = cfgPetLabels cfg ++ cfgAudioLabels cfg ++ cfgPersonLabels cfg

-- | A hair below Frigate's start_time resolution, for re-including boundary ties. The
-- watermark is nudged this far below the last event, so the next pass, whose @after@ is
-- strictly exclusive, still re-fetches any event sharing that exact start_time.
-- 'Db.eventStored' dedupes the re-fetch for free.
cursorEpsilon :: Double
cursorEpsilon = 1e-3

-- | Where the event watermark lands after one window pass. Pure, so its edges are
-- unit-tested. @lo@ is the window's lower bound and the fold's seed, @frozen@ says whether
-- the fold stopped early, @got@ is how many events the page returned, @foldMax@ the fold's
-- max start_time, and @hi@ the window's upper bound.
--
-- A frozen fold or a FULL page both mean more events remain at or above @foldMax@, so the
-- watermark holds there, nudged below by 'cursorEpsilon' so a same-start_time tie split
-- across the boundary is re-fetched rather than skipped by the exclusive @after@.
--
-- That nudge is clamped at @lo@. A fold that froze before advancing past any event has
-- @foldMax == lo@, and @lo - cursorEpsilon@ would drift the persisted cursor backward a
-- millisecond per starved pass. Only a short page that drained cleanly advances to @hi@,
-- which also steps past a quiet, event-free gap.
advanceWatermark :: Double -> Bool -> Int -> Double -> Double -> Double
advanceWatermark lo frozen got foldMax hi
  | frozen || got >= eventFetchLimit = max lo (foldMax - cursorEpsilon)
  | otherwise                        = max foldMax hi

-- | Which stage is spending, so the budget knows which instant applies.
data Stage = Events | Samples
  deriving stock (Eq, Show)

-- | A batch's allowance, measured in time rather than in items.
--
-- Items are the wrong unit: a queued frame is one picture, an event is a snapshot plus up to
-- 'eventClipFrames' more, so the same item count is anywhere from a minute of work to hours
-- of it (see 'batchDeadlineSecs', which exists because the old item cap could not bound
-- time). What actually needs bounding is how long the single batch worker is occupied, since
-- every on-demand refresh and day rebuild queues behind it. So bound that directly.
data Budget = Budget
  { bgEventsUntil :: UTCTime
  -- ^ Events may start a new item before this. Its only job is to protect the samples that
  -- run after them: the stages are sequential, so events cannot themselves be starved, and
  -- capping them here leaves the rest of the window for samples however long ingest wants.
  , bgDeadline    :: UTCTime
  -- ^ The whole batch's ceiling, and the samples' limit.
  , bgSpent       :: IORef Int
  -- ^ Items done, for the outcome note only. It does not gate anything.
  , bgClock       :: Clock.Handle
  }

-- | Build a fresh batch allowance: the event cut of the window, then the window itself.
newBudget :: App -> IO Budget
newBudget = budgetWith eventWindowShare

-- | An allowance for a run with no sample stage, so events get the whole window.
--
-- The event cut exists only to leave room for the samples that follow. A past-day rebuild
-- never touches the frame queue, so applying it there would idle the last 30% of the window
-- for nobody, and stop a rebuild that had real work left with minutes still on the clock.
newEventBudget :: App -> IO Budget
newEventBudget = budgetWith 1

budgetWith :: Rational -> App -> IO Budget
budgetWith share app = do
  now <- Clock.now (appClock app)
  spent <- newIORef 0
  pure
    Budget
      { bgEventsUntil = addUTCTime (fromRational (share * toRational batchDeadlineSecs)) now
      , bgDeadline = addUTCTime batchDeadlineSecs now
      , bgSpent = spent
      , bgClock = appClock app
      }

-- | The events' cut of the batch window. The remainder is what samples are guaranteed, so
-- this is really "how much of the window ingest may take before the fallback gets its turn".
-- A dimensionless fraction, not a duration, hence 'Rational' rather than the
-- 'NominalDiffTime' it is multiplied by.
eventWindowShare :: Rational
eventWindowShare = 7 / 10

-- | How many items the batch got through, for the outcome note.
budgetUsed :: Budget -> IO Int
budgetUsed = readIORef . bgSpent

-- | May this stage start another item? 'False' once its instant has passed. Checked before
-- each item, so a large backlog is deferred rather than read into memory.
--
-- One item can overshoot its limit, since the check happens before the work rather than
-- during it. That is bounded by a single item's duration and is the same behaviour the
-- deadline has always had.
--
-- Time also spills the right way for free. Ingest finishing early leaves the rest of the
-- window to samples, because samples measure against the outer deadline and never against
-- what events did or did not use.
takeBudget :: Stage -> Budget -> IO Bool
takeBudget stage b = do
  now <- Clock.now (bgClock b)
  if now >= limit
    then pure False
    else do
      atomicModifyIORef' (bgSpent b) (\n -> (n + 1, ()))
      pure True
  where
    limit = case stage of
      Events  -> bgEventsUntil b
      Samples -> bgDeadline b

-- | The pipeline's component tracer.
ptrace :: App -> Tracer IO PipelineEvent
ptrace = pipelineTracer . appTracer

-- --------------------------------------------------------------------------- --
-- Capture
-- --------------------------------------------------------------------------- --

-- | The capture half of the pipeline: queue one clean frame per online enabled camera, with
-- no model call. 'PetReport.Pipeline.Scheduler.runCaptureScheduler' runs it on its interval,
-- and the frames sit in the per-camera queue until the next batch's 'analyzeQueue' drains
-- them.
capture :: App -> IO ()
capture app = do
  let cfg = appConfig app
  -- Use the profile's enabled cameras, the same list analyzeQueue and pruneQueue drain,
  -- rather than cfgCameras. applyProfile falls back to the env camera list when the profile
  -- enables none, which would queue frames the drain never reads.
  prof <- Db.getProfile (appDb app)
  online <- Frigate.onlineCameras (appFrigate app) (enabledCameras prof)
  now <- Clock.now (appClock app)
  -- A sample exists to cover a stretch Frigate said nothing about, so a camera Frigate has
  -- just reported on does not need one: the event carries a clip and a real detection, which
  -- a blind snapshot of the same room does not. Skipping those keeps the queue to genuinely
  -- quiet cameras instead of a fixed frame-per-camera-per-interval that no batch can drain.
  -- No online camera means nothing to sample either way, so skip the quiet check rather than
  -- spending a Frigate request every pass to learn that.
  busy <- if null online then pure Set.empty else busyCameras app (posixSecs now)
  let cams = filter (`Set.notMember` busy) online
      ts = round (utcTimeToPOSIXSeconds now) :: Integer
  -- One independent Frigate fetch and queue-write per camera. They are network-bound and
  -- write to distinct directories, so run them together rather than one after another.
  saved <- sum
    <$> Async.mapConcurrently
      ( \cam -> do
          m <- Frigate.latestFrame (appFrigate app) cam
          case m of
            Just jpg -> writeQueueFrame cfg cam ts jpg >> pure (1 :: Int)
            Nothing  -> pure 0
      )
      cams
  traceWith (ptrace app) (FramesQueued saved (length cams))

-- | The cameras Frigate has reported an event on within 'captureQuietSecs', which therefore
-- need no blind sample this pass.
--
-- One request covering every camera, grouped here, rather than one per camera: 'capture'
-- already fans out per camera for the frame fetch, and this would multiply that.
--
-- A failed fetch reads as "nobody is busy", so a Frigate blip degrades to sampling everything
-- (the old behaviour) rather than to sampling nothing. Silence is the wrong default when the
-- whole point of a sample is to cover a gap. 'eventFetchLimit' is reused as the page size
-- here; only the distinct camera set matters, so a truncated page can at worst leave a busy
-- camera looking quiet and earn it one redundant frame.
busyCameras :: App -> Double -> IO (Set.Set Text)
busyCameras app nowP = do
  mevents <-
    Frigate.recentEvents
      (appFrigate app)
      (ingestLabels (appConfig app))
      (nowP - captureQuietSecs)
      nowP
      eventFetchLimit
  pure (maybe Set.empty (Set.fromList . map feCamera) mevents)

-- --------------------------------------------------------------------------- --
-- Batch
-- --------------------------------------------------------------------------- --

-- | Run the batch pipeline under the in-process batch mutex, so it cannot duplicate work by
-- running alongside an on-demand rebuild or an inline cleanup-now.
batch :: App -> IO ()
batch app = withBatchLock app $ do
  -- The profile is read here rather than inside 'batchSteps' because the budget is sized
  -- from the enabled camera count, so it has to exist before the budget does.
  prof <- Db.getProfile (appDb app)
  budget <- newBudget app
  recordingOutcome app (batchNote budget) (batchSteps app budget prof)
  where
    batchNote budget secs = do
      used <- budgetUsed budget
      pure ("processed " <> tshow used <> " item(s) in " <> tshow secs <> "s")

-- | Build one day's report on demand, either its first build or a refresh. Ingests that
-- day's own event window straight from Frigate WITHOUT moving the global watermark, then
-- synthesises the report silently, since a backfilled day should not push a notification.
--
-- Already-stored events dedup, so a rebuild is idempotent and independent of every other
-- day. It skips the today-and-global steps of a full batch: queue analysis, pruning, weekly
-- summaries. Runs under the in-process batch mutex, so it cannot race a concurrent batch.
buildDay :: App -> Day -> IO ()
buildDay app day = withBatchLock app $ do
  -- buildDay shares the batch budget mechanism, so a heavy past-day rebuild is
  -- deadline-bounded too and defers its remainder instead of running unbounded.
  budget <- newEventBudget app
  recordingOutcome app (\secs -> pure ("built " <> tshow day <> " in " <> tshow secs <> "s")) $ do
    prof <- Db.getProfile (appDb app)
    tz <- Clock.timeZone (appClock app)
    now <- Clock.now (appClock app)
    -- Ingest just this day's own event window, straight from Frigate, WITHOUT moving the
    -- global last_event_ts watermark. A past day the steady-state ingest already swept past,
    -- or never reached, is rebuilt from its own bounds, and already-stored events are
    -- skipped, so a rebuild is idempotent and independent of every other day.
    let (loT, hiT) = localDayWindow tz now day
        -- Nudge the lower bound below local midnight, so an event landing exactly on it is
        -- not lost to the exclusive @after@. Harmless here, since the watermark is
        -- discarded and eventStored dedups the overlap.
        lo = posixSecs loT - cursorEpsilon
        hi = posixSecs hiT
    end <- sweepWindow app budget prof lo hi
    finishReportFor app prof day False
    -- Marked only once the day's own events are all in AND its story is written, so a
    -- rebuild that stopped short, or whose narrative call failed, leaves the day open: the
    -- notice still says so and a second press picks up where this one stopped.
    when (end >= hi) (Db.setDaySwept (appDb app) day)

-- | Ingest @[lo, hi)@ page by page until it stops moving. Returns where the cursor stopped,
-- which is @hi@ exactly when the window drained.
--
-- 'ingestWindow' takes one page of 'eventFetchLimit' per call and parks the cursor below the
-- last event of a full page, since a full page means there may be more. Left at that, a run
-- stops after 500 events however cheap they were: an event already stored, cooldown-skipped
-- or flagged a false positive advances the cursor for free, so a backlog of work already done
-- would still cost one run per 500 to walk past, at twelve hours a run for the daily batch.
-- The budget bounds the work, so the page count does not have to.
--
-- Terminates on any of the three ways forward stops: reaching @hi@, a budget with nothing
-- left to give, or a fetch that failed. Each leaves the cursor at or below where its page
-- began. The number of pages is bounded by the window, which 'clampToRetention' keeps inside
-- Frigate's own retention.
sweepWindow :: App -> Budget -> Profile -> Double -> Double -> IO Double
sweepWindow app budget prof lo hi = go lo
  where
    go from = do
      to <- ingestWindowSafe app budget prof from hi
      if to >= hi || to <= from then pure to else go to

-- | How a batch run ended, for the honest last-batch state the UI reads. The wire
-- form is "ok", "error" or "skipped" (see 'runOutcomeText'), which is what @batchH@ stores
-- and reads back. The sum type just pins that vocabulary in Haskell.
data RunOutcome = RanOk | RanError | RanSkipped
  deriving stock (Eq, Show)

-- | The stored status string for a run outcome. The last-batch state depends on this closed
-- vocabulary, so keep it byte-identical.
runOutcomeText :: RunOutcome -> Text
runOutcomeText o = case o of
  RanOk      -> "ok"
  RanError   -> "error"
  RanSkipped -> "skipped"

-- | Run an action while holding an in-process mutex, without blocking: 'Just' the result if
-- the lock was free, 'Nothing' if it was already held. The 'MVar' is full when free, and
-- 'bracket' restores it even if @act@ throws. In-process is enough, the whole app being one
-- serve process.
withTryLock :: MVar () -> IO a -> IO (Maybe a)
withTryLock mv act = bracket (tryTakeMVar mv) release run
  where
    run (Just ()) = Just <$> act
    run Nothing   = pure Nothing
    release (Just ()) = putMVar mv ()
    release Nothing   = pure ()

-- | Run an action under the in-process batch mutex. When another run already holds it, be
-- that the scheduled batch, an on-demand rebuild or an inline cleanup-now, skip rather than
-- block, and record the skip. Without that record the UI would keep showing the previous
-- run's "ok" and read as a false "just updated".
withBatchLock :: App -> IO () -> IO ()
withBatchLock app act = do
  r <- withTryLock (appBatchLock app) act
  case r of
    Just () -> pure ()
    Nothing -> do
      now <- Clock.now (appClock app)
      recordBatch app now RanSkipped "another run is already in progress"
      traceWith (ptrace app) BatchAlreadyRunning

-- | Time an action and persist its outcome, @ok@ with a note or @error@, so the UI shows an
-- honest refresh state rather than a silently-stopped spinner. Rethrows on failure, so the
-- worker's tracing still fires.
recordingOutcome :: App -> (Int -> IO Text) -> IO () -> IO ()
recordingOutcome app mkNote act = do
  start <- Clock.now (appClock app)
  r <- try act :: IO (Either SomeException ())
  done <- Clock.now (appClock app)
  let secs = round (realToFrac (diffUTCTime done start) :: Double) :: Int
  case r of
    Right () -> mkNote secs >>= recordBatch app done RanOk
    Left e   -> recordBatch app done RanError (T.take 200 (tshow e)) >> throwIO e

batchSteps :: App -> Budget -> Profile -> IO ()
batchSteps app budget prof = do
  refreshBriefStep app prof
  -- Events BEFORE queued samples, and the order is load-bearing rather than incidental.
  -- Both draw on the one budget, so whichever runs first can spend all of it. Capture
  -- queues a frame per camera per interval whether or not anything happened, which
  -- outpaces what a batch can analyse, so draining the queue first starves ingest
  -- permanently: the watermark freezes below the first event and never moves again.
  -- Samples exist to fill the gaps between events, so events are the signal and samples
  -- take the remainder.
  late <- ingestEvents app budget prof
  analyzeQueue app budget prof
  -- After BOTH stages, since a frame queued last night lands on yesterday exactly as an
  -- event does. Repairing between them would rewrite yesterday's story and then bury it
  -- under the samples that arrived a moment later.
  repairLateDays app prof late
  pruneProof app prof
  pruneQueue app prof
  finishReport app prof
  materializeRollups app
  void (garbageCollect app prof)
  writePetSummaries app prof

-- | Persist the last batch's time, status and a short note, for the web layer to report. A
-- small JSON blob in the @state@ table. @batchH@ reads it back opaquely as a Value, so the
-- wire string from 'runOutcomeText' is the part that matters.
recordBatch :: App -> UTCTime -> RunOutcome -> Text -> IO ()
recordBatch app at' status note =
  Db.setState
    (appDb app)
    "last_batch"
    (TL.toStrict (encodeToLazyText (object ["at" .= at', "status" .= runOutcomeText status, "note" .= note])))

-- | Keep the curated identification brief in step with the roster. A hash check regenerates
-- only when stale, which makes this idempotent and lets it self-heal a brief whose
-- background regen was dropped as a duplicate by a rapid second settings save. A model
-- hiccup here must not fail the batch.
refreshBriefStep :: App -> Profile -> IO ()
refreshBriefStep app prof =
  handle onErr (IdentGuide.refreshIfStale (appLlm app) (appDb app) prof)
  where
    onErr (e :: SomeException) =
      traceWith (ptrace app) (BriefRefreshFailed (tshow e))

-- | Analyse queued sample frames. A model outage aborts and leaves the frames queued. An
-- empty-room frame is discarded; a frame with a pet in it is kept as proof and stored.
analyzeQueue :: App -> Budget -> Profile -> IO ()
analyzeQueue app budget prof =
  handle onErr $ do
    brief <- IdentGuide.identGuide (appDb app) prof
    queues <- mapM pending (enabledCameras prof)
    -- Round-robin rather than draining each camera in turn. Sequentially, a share that runs
    -- out is spent entirely on whichever camera sorts first, and the last camera is never
    -- analysed at all: the same starvation as events-versus-samples, one level down and
    -- with a stable order, so the same camera loses every batch. Interleaving spreads a
    -- short share evenly, oldest frame of each camera first.
    forM_ (concat (transpose queues)) (analyseFrame brief)
  where
    cfg = appConfig app
    onErr (e :: SomeException) =
      traceWith (ptrace app) (ModelUnavailable (tshow e))
    pending cam = do
      files <- sort <$> listJpgs (cfgQueueDir cfg </> T.unpack cam)
      pure [(cam, f) | f <- mapMaybe withTs files]
    analyseFrame brief (cam, (fname, ts)) = do
      ok <- takeBudget Samples budget
      -- Over the samples' share OR past the wall-clock deadline, leave the frame queued for
      -- the next run. Checked before reading the file, so a large backlog is not all pulled
      -- into memory.
      if not ok
        then pure ()
        else do
          let qdir = cfgQueueDir cfg </> T.unpack cam
              qpath = qdir </> fname
          mjpg <- tryRead qpath
          case mjpg of
            Nothing -> pure ()
            Just jpg -> do
              mscene <- Vision.analyze (appLlm app) brief [jpg]
              case mscene of
                -- Unparsed response, so leave the frame queued to retry. pruneQueue caps
                -- genuinely-stuck frames, so they cannot linger forever.
                Nothing -> pure ()
                Just scene
                  | hasPet scene -> do
                      moveToProof cfg cam fname
                      Db.insertObservation (appDb app) (sampleObs ts cam scene)
                  | otherwise -> removeQuiet qpath

sampleObs :: Integer -> Text -> Scene -> NewObservation
sampleObs ts cam scene =
  NewObservation
    { noAt = posixSecondsToUTCTime (fromInteger ts)
    , noCamera = Camera cam
    , noOrigin = PeriodicSample
    , noPerception = Seen scene
    }

-- | The daily batch's event catch-up: advance the global watermark through the events since
-- it, oldest-first, then persist it. A normal batch needs a single request; a long absence
-- keeps paging until the work, not the page count, runs out (see 'sweepWindow').
--
-- Returns the past days this pass could still add to, each with what it held beforehand, for
-- 'repairLateDays' to compare against once the batch has finished storing. The pair is taken
-- here because only this function knows the window: the watermark it starts from is clamped
-- to Frigate's retention, so a caller sampling the stored value would name the wrong days.
ingestEvents :: App -> Budget -> Profile -> IO [(Day, Int)]
ingestEvents app budget prof = do
  stored <- Db.getIngestWatermark (appDb app)
  now <- Clock.now (appClock app)
  tz <- Clock.timeZone (appClock app)
  let nowP = posixSecs now
  wm <- clampToRetention app nowP stored
  let stale = lateDayCandidates tz now wm
  before <- traverse (countObsOn app tz now) stale
  newWm <- sweepWindow app budget prof wm nowP
  Db.setIngestWatermark (appDb app) newWm
  -- Reaching the window's end means no backlog is left; parking below it means the budget or
  -- the event volume stopped this pass short. The current day's notice reads this rather than
  -- the watermark, so a normal between-batch lag does not read as falling behind.
  Db.setIngestDrained (appDb app) (newWm >= nowP)
  pure (zip stale before)

-- | Rewrite the story of any past day this batch actually added to.
--
-- 'finishReport' only ever writes today, so a day first reported while it was still
-- half-finished keeps the narrative it was given: its timeline and its stats heal on their
-- own as the missing moments land, its prose never does.
--
-- Counted rather than assumed, because the ingest window always reaches back into yesterday.
-- Repairing every day it touched would re-narrate yesterday every single morning, at a model
-- call each, to say the same thing again. Silent, too: the owner was told about that day when
-- it was current, and a correction is not news.
repairLateDays :: App -> Profile -> [(Day, Int)] -> IO ()
repairLateDays app prof before = do
  now <- Clock.now (appClock app)
  tz <- Clock.timeZone (appClock app)
  forM_ before $ \(day, was) -> do
    is <- countObsOn app tz now day
    when (is > was) (finishReportFor app prof day False)

-- | The past days an ingest window starting at @wm@ can still add observations to, oldest
-- first, today excluded.
--
-- Capped so a long backlog cannot turn one batch into a run of narrative calls. The rest are
-- not lost: each batch takes the next few, so a ten-day gap heals over a few runs.
lateDayCandidates :: TZ -> UTCTime -> Double -> [Day]
lateDayCandidates tz now wm =
  let from = localDayOf tz (posixSecondsToUTCTime (realToFrac wm))
      today = localDayOf tz now
   in take maxLateDayRepairs (takeWhile (< today) [addDays k from | k <- [0 ..]])

-- | How many past days one batch will re-narrate.
maxLateDayRepairs :: Int
maxLateDayRepairs = 3

countObsOn :: App -> TZ -> UTCTime -> Day -> IO Int
countObsOn app tz now day =
  let (dayStart, dayEnd) = localDayWindow tz now day
   in length <$> Db.observationsBetween (appDb app) dayStart dayEnd

-- | Floor the watermark at Frigate's own media-retention horizon.
--
-- Past that horizon the clips and snapshots are gone, so those events can only be fetched,
-- found media-less and skipped, and each one still costs a budget unit on the way. A
-- watermark left far behind (ingest wedged, or the service down for a fortnight) would
-- otherwise spend batch after batch grinding through history that can no longer be analysed,
-- while the events happening now wait behind it.
--
-- Retention comes from 'appRetention', which the poller keeps fresh and which a failed fetch
-- leaves untouched rather than blanking. The MAXIMUM across cameras is the floor, not the
-- minimum, so an event still recoverable from the longest-retaining camera is not discarded
-- for the sake of the shortest. An empty map means retention is simply unknown, in which
-- case guessing a floor could silently discard real history, so nothing is clamped.
-- 'Nothing' is a watermark that has never been set, which is a fresh install rather than a
-- stall. Kept as a 'Maybe' rather than collapsed to 0, so "never ingested" and "ingested at
-- the epoch" stay distinguishable and the warning below can tell them apart.
clampToRetention :: App -> Double -> Maybe Double -> IO Double
clampToRetention app nowP mwm = do
  retain <- readTVarIO (appRetention app)
  case Map.elems retain of
    -- No floor can be justified, so honour what is stored. A fresh install then sweeps from
    -- 0, which is slow but complete, and the next pass has retention to clamp with.
    [] -> pure (fromMaybe 0 mwm)
    days
      | Just wm <- mwm, wm >= horizon -> pure wm
      | otherwise -> do
          -- Only an actual stall is worth warning about. A fresh install has no watermark,
          -- and starting at the horizon is simply where it should start.
          forM_ mwm $ \wm ->
            traceWith (ptrace app) (IngestWatermarkStale (round ((nowP - wm) / 86400)))
          pure horizon
      where
        horizon = nowP - maximum days * 86400

-- | 'ingestWindow' made resilient. A Frigate or model hiccup mid-pass aborts and is traced
-- rather than propagated, so a batch or a past-day rebuild still finishes its report, and
-- any events already stored stay stored. Aborting returns the low bound, leaving the
-- watermark where it was for the next run to retry.
ingestWindowSafe :: App -> Budget -> Profile -> Double -> Double -> IO Double
ingestWindowSafe app budget prof lo hi =
  handle onErr (ingestWindow app budget prof lo hi)
  where
    onErr (e :: SomeException) =
      lo <$ traceWith (ptrace app) (EventIngestFailed (tshow e))

-- | Ingest pet and audio events whose start falls in the half-open window @[lo, hi)@,
-- oldest-first, returning the watermark the window resolves to. The daily batch persists
-- that; a per-day rebuild discards it.
--
-- Frigate returns events oldest-first, so the fold advances the watermark contiguously from
-- @lo@ and can only ever defer a NEWER suffix, never skip an older event. A frozen fold
-- keeps its watermark so the deferred remainder is re-fetched next pass, and a FULL page
-- likewise means more remain above it. Only a short page that drained cleanly advances to
-- @hi@, which also steps past a quiet, event-free gap.
ingestWindow :: App -> Budget -> Profile -> Double -> Double -> IO Double
ingestWindow app budget prof lo hi
  | hi <= lo = pure lo
  | otherwise = do
      mevents <- Frigate.recentEvents (appFrigate app) (ingestLabels (appConfig app)) lo hi eventFetchLimit
      case mevents of
        -- A failed fetch, NOT an empty window. Hold the watermark so this range is retried
        -- next pass, rather than advancing past events we never saw.
        Nothing -> pure lo
        Just events -> do
          -- Log the window's yield before folding, so the per-event skip traces below have a
          -- denominator: an empty window (0 returned) reads differently from a full one whose
          -- events were every one skipped.
          traceWith (ptrace app) (EventsFetched (length events) (round lo) (round hi))
          nowP <- posixSecs <$> Clock.now (appClock app)
          -- The brief does not change across the pass, so fetch it once rather than per
          -- event.
          brief <- IdentGuide.identGuide (appDb app) prof
          -- Seed the cooldown map from events already stored in the lookback just below
          -- @lo@. An empty seed each pass would let the first same-key event of a new page
          -- through even with one stored seconds earlier, so a burst split across the
          -- boundary would bypass the cooldown entirely.
          seedRows <- Db.recentEventStarts (appDb app) (lo - eventCooldownSec) lo
          let seed = Map.fromList [((cam, lbl), ts) | (cam, lbl, ts) <- seedRows]
          -- Events arrive oldest-first, so the fold's max is the newest one processed, and
          -- 'advanceWatermark' decides where the cursor lands.
          (_, wm, frozen) <- foldM (step nowP brief) (seed, lo, False) events
          let newWm = advanceWatermark lo frozen (length events) wm hi
          -- Events came back but the watermark moved past none of them, so the next pass
          -- re-fetches this exact window. Once is ordinary (a budget ran out, media was not
          -- ready). Every pass means ingest is wedged and nothing will ever be recorded.
          when (not (null events) && newWm <= lo) $
            traceWith (ptrace app) (IngestStalled (length events))
          pure newWm
  where
    -- Fold over events in start order. The watermark advances past anything already
    -- stored, freshly stored, or deliberately cooldown-skipped, but freezes below the first
    -- transiently-failed event, so that event and everything after it are re-fetched next
    -- pass. The unique event_id index and INSERT OR IGNORE make re-storing an already
    -- handled event a no-op, so holding the line cannot duplicate. A failure older than the
    -- retry window is abandoned, so one permanently-unreadable event cannot wedge ingestion.
    step ::
      Double ->
      Text ->
      (Map (Text, Text) Double, Double, Bool) ->
      FrigateEvent ->
      IO (Map (Text, Text) Double, Double, Bool)
    step nowP brief (seen, wm, frozen) ev
      | feFalsePositive ev = do
          traceWith (ptrace app) (EventSkipped SkippedFalsePositive (feId ev) (feLabel ev))
          -- A Frigate-confirmed non-detection. Advance past it, but leave the cooldown map
          -- alone, since it is not a real sighting, and spend no budget or vision on it.
          pure (seen, adv wm, frozen)
      | otherwise = do
          already <- Db.eventStored (appDb app) (feId ev)
          if already
            -- Already ingested, from a re-fetched window edge or a rebuild over a day we
            -- hold. Advance past it for free: no snapshot, no vision, no budget.
            then pure (Map.insert key (feStart ev) seen, adv wm, frozen)
            else if not coolOk
              then do
                traceWith (ptrace app) (EventSkipped SkippedCooldown (feId ev) (feLabel ev))
                pure (seen, adv wm, frozen)
              else do
                -- Only events past cooldown do real work, so only they take budget. Over
                -- the item cap OR past the wall-clock deadline, freeze the watermark so
                -- this and later events retry next pass instead of making one run
                -- unbounded. A deadline stop and a budget stop are the same thing here:
                -- both defer the remainder through the frozen watermark, leaving the events
                -- in Frigate to re-fetch.
                ok <- takeBudget Events budget
                if not ok
                  then pure (seen, wm, True)
                  else do
                    stored <- ingestOne app brief ev
                    if stored
                      then pure (Map.insert key (feStart ev) seen, adv wm, frozen)
                      else
                        if nowP - feStart ev > eventRetryWindowSec
                          then do
                            traceWith (ptrace app) (EventSkipped SkippedAbandoned (feId ev) (feLabel ev))
                            pure (seen, adv wm, frozen)
                          else pure (seen, wm, True)
      where
        key = (feCamera ev, feLabel ev)
        coolOk = maybe True (\l -> feStart ev - l >= eventCooldownSec) (Map.lookup key seen)
        adv w = if frozen then w else max w (feStart ev)

-- | Analyse and store one Frigate event. 'True' means handled, whether stored or with
-- nothing to analyse, so the watermark may advance past it. 'False' means a transient
-- failure, media not ready or an unparsed model response, so the fold freezes below it and
-- the event is re-fetched next pass.
ingestOne :: App -> Text -> FrigateEvent -> IO Bool
ingestOne app brief ev
  -- An audio detection stores directly as a sound observation, with no vision call.
  | feLabel ev `elem` cfgAudioLabels (appConfig app) = do
      Db.insertObservation (appDb app) (soundObs ev)
      pure True
  | otherwise = case Frigate.eventMedia ev of
      -- Frigate reports no snapshot and no clip. A COMPLETED event has nothing to analyse,
      -- so treat it as handled and let the watermark advance; a snapshot fetch would only
      -- 404, and reading that as a transient failure would freeze ingest for the whole
      -- retry window. A still-open event may yet get its media, so hold the watermark below
      -- it and retry, exactly as a not-ready snapshot does.
      Frigate.NoMedia
        | isJust (feEnd ev) -> do
            traceWith (ptrace app) (EventSkipped SkippedNoMedia (feId ev) (feLabel ev))
            pure True
        | otherwise         -> pure False
      media -> do
        msnap <- Frigate.eventSnapshot (appFrigate app) (feId ev)
        clips <- case media of
          Frigate.HasClip -> do
            mclip <- Frigate.eventClip (appFrigate app) (feId ev)
            maybe (pure []) (\c -> Ffmpeg.clipFrames (appFfmpeg app) c eventClipFrames) mclip
          _ -> pure []
        let frames = maybe [] pure msnap ++ clips
        if null frames
          -- Media was expected but the fetch came back empty, typically a snapshot not
          -- written yet. A genuine transient, so freeze and retry within the window.
          then pure False
          else do
            mscene <- Vision.analyze (appLlm app) brief frames
            case mscene of
              Nothing -> pure False
              Just scene -> do
                Db.insertObservation (appDb app) (eventObs ev scene)
                pure True

soundObs :: FrigateEvent -> NewObservation
soundObs ev =
  NewObservation
    { noAt = posixSecondsToUTCTime (realToFrac (feStart ev))
    , noCamera = Camera (feCamera ev)
    , noOrigin = FromEvent (FrigateMeta (EventId (feId ev)) (feLabel ev) (feScore ev) (feHasClip ev) (feHasSnapshot ev))
    , noPerception = Heard (SoundKind (feLabel ev))
    }

eventObs :: FrigateEvent -> Scene -> NewObservation
eventObs ev scene =
  NewObservation
    { noAt = posixSecondsToUTCTime (realToFrac (feStart ev))
    , noCamera = Camera (feCamera ev)
    , noOrigin =
        FromEvent
          (FrigateMeta (EventId (feId ev)) (feLabel ev) (feScore ev) (feHasClip ev) (feHasSnapshot ev))
    , noPerception = Seen scene
    }

-- | Remove timestamped @.jpg@ frames older than @retainDays@ from a per-camera directory,
-- the config accessor picking proof or queue. One body for both prune passes, which differ
-- only in the directory and the retention constant. @keep@ protects a frame from pruning
-- even when it is old, which is how a kept sample is spared.
pruneDir :: App -> Profile -> (Config -> FilePath) -> Double -> ((Text, Integer) -> Bool) -> IO ()
pruneDir app prof dirOf retainDays keep = do
  let cfg = appConfig app
  now <- Clock.now (appClock app)
  let cutoff = addUTCTime (negate (realToFrac (retainDays * 86400))) now
  forM_ (enabledCameras prof) $ \cam -> do
    let dir = dirOf cfg </> T.unpack cam
    files <- listJpgs dir
    forM_ (mapMaybe withTs files) $ \(fname, ts) ->
      let t = posixSecondsToUTCTime (fromInteger ts)
       in unless (t >= cutoff || keep (cam, ts)) (removeQuiet (dir </> fname))

-- | Prune proof frames past the retention window, sparing a kept sample's frame. Keeping a
-- moment takes ownership of its media, so it has to survive the pruner. An event's media is
-- copied into owned storage, but a sample's proof frame IS the owned copy.
pruneProof :: App -> Profile -> IO ()
pruneProof app prof = do
  kept <- Set.fromList <$> Db.keptSampleStamps (appDb app)
  pruneDir app prof cfgProofDir proofRetainDays (`Set.member` kept)

-- | Drop queued sample frames older than a couple of days. A frame is normally analysed and
-- moved within a batch or two, so this only catches the ones a model keeps failing to parse,
-- which would otherwise linger and be re-analysed forever.
pruneQueue :: App -> Profile -> IO ()
pruneQueue app prof = pruneDir app prof cfgQueueDir queueRetainDays (const False)

-- | How many completed local days back 'materializeRollups' refreshes each batch.
rollupBackfillDays :: Integer
rollupBackfillDays = 3

-- | Re-materialise the durable per-pet stats rollup for the last 'rollupBackfillDays'
-- completed local days on each batch, so a correction to a recent day shows up and every day
-- has a durable aggregate well before garbage collection reaches it. Today is skipped, since
-- it is still accumulating and computed live until it completes.
materializeRollups :: App -> IO ()
materializeRollups app = do
  tz <- Clock.timeZone (appClock app)
  now <- Clock.now (appClock app)
  let today = localDayOf tz now
  forM_ [1 .. rollupBackfillDays] $ \d -> do
    let day = addDays (negate d) today
        (lo, hi) = localDayWindow tz now day
    Db.materializeDay (appDb app) (dayKeyText day) lo hi

-- | The GC window in whole days: the owner's @gcWindowDays@, at least 1. Nothing floors it,
-- so shortening the window actually takes effect. A moment's countdown ('momentExpiry')
-- shows whichever comes first, GC removing the record or Frigate pruning the clip, so the
-- owner is never surprised.
gcEffectiveWindow :: Int -> Integer
gcEffectiveWindow = max 1 . fromIntegral

-- | Garbage-collect old un-kept moments. Every day past the window that still has un-kept
-- moments gets its durable rollup materialised FIRST, from the full day, then its un-kept
-- moments deleted. The day's per-pet stats and any kept moments survive while the clip-less
-- clutter goes.
--
-- A day is only processed while it still has un-kept moments, so an already-collected day is
-- skipped and its rollup is never rebuilt from the partial remainder. Returns the total
-- collected, so a manual run can report it.
garbageCollect :: App -> Profile -> IO Int
garbageCollect app prof = do
  tz <- Clock.timeZone (appClock app)
  now <- Clock.now (appClock app)
  mrange <- Db.tsRange (appDb app)
  -- Collect only days whose LATEST moment is already past the window. A day is a full
  -- local day, so its last moment is up to a day younger than the day itself, and the extra
  -- day guarantees every moment in a collected day is at least @effWin@ days old.
  let effWin = gcEffectiveWindow (gcWindowDays prof)
      newestCollectable = addDays (negate (effWin + 1)) (localDayOf tz now)
  case mrange of
    Just (lo0, _) ->
      foldM
        ( \acc day -> do
            let (lo, hi) = localDayWindow tz now day
            hasUnkept <- Db.hasUnkeptBetween (appDb app) lo hi
            if hasUnkept
              then do
                Db.materializeDay (appDb app) (dayKeyText day) lo hi
                n <- Db.collectUnkept (appDb app) lo hi
                traceWith (ptrace app) (GcCollected n (dayKeyText day))
                pure $! acc + n
              else pure acc
        )
        0
        [localDayOf tz lo0 .. newestCollectable]
    Nothing -> pure 0

-- | Garbage-collect under the batch mutex, so a manual "run cleanup now" cannot race the
-- scheduled batch's GC. Racing would let one run's 'materializeDay' rebuild a day's rollup
-- from the other run's partial moments. 'Nothing' when a batch is already running,
-- otherwise 'Just' the collected count.
garbageCollectNow :: App -> Profile -> IO (Maybe Int)
garbageCollectNow app prof = withTryLock (appBatchLock app) (garbageCollect app prof)

-- | The daily batch's report step: today's report, notifying on a fresh one.
finishReport :: App -> Profile -> IO ()
finishReport app prof = do
  tz <- Clock.timeZone (appClock app)
  now <- Clock.now (appClock app)
  finishReportFor app prof (localDayOf tz now) True

-- | Synthesise and store the report for a given local day. Drives both the daily batch,
-- which does today and notifies, and an on-demand past-day rebuild, which is silent.
--
-- The window runs from that local day up to now, so a partial "today" still works. A past
-- day is complete, so it files under Evening. A push goes out only when @notify@ is set and
-- this is the first report for that @(day, period)@; a re-run refreshes the prose in place.
finishReportFor :: App -> Profile -> Day -> Bool -> IO ()
finishReportFor app prof day notify = do
  let clk = appClock app
  now <- Clock.now clk
  tz <- Clock.timeZone clk
  let today = localDayOf tz now
      (dayStart, dayEnd) = localDayWindow tz now day
  obss <- Db.observationsBetween (appDb app) dayStart dayEnd
  if null obss
    then traceWith (ptrace app) NoObservationsForReport
    else do
      brief <- IdentGuide.identGuide (appDb app) prof
      narrative <- Narrative.synthesize (appLlm app) Llm.backgroundBudget tz prof brief obss
      let hr = todHour (localTimeOfDay (utcToLocalTimeTZ tz now))
          per = if day == today && hr < 14 then Morning else Evening
          alert = any concerningObs obss
          prio = if alert then 4 else 3 :: Int
          tags = if alert then "paw_prints,warning" else "paw_prints"
      alreadySent <- Db.reportExists (appDb app) day per
      Db.insertReport
        (appDb app)
        Report
          { reportDay = day
          , period = per
          , narrative = narrative
          , reportAt = now
          }
      if notify && not alreadySent
        then do
          Ntfy.push (appNtfy app) (Ntfy.Notification "Pet report" narrative prio tags)
          traceWith (ptrace app) (ReportStored True)
        else traceWith (ptrace app) (ReportStored False)

-- --------------------------------------------------------------------------- --
-- Per-pet weekly summaries
-- --------------------------------------------------------------------------- --

-- | For each pet, compute the deterministic weekly insights and ask the model for a warm
-- recap, caching the result. A model outage skips them all, and the stats still render
-- without the prose.
writePetSummaries :: App -> Profile -> IO ()
writePetSummaries app prof = handle onErr $ do
  let clk = appClock app
      roster = pets prof
      crs = cameras prof
  now <- Clock.now clk
  tz <- Clock.timeZone clk
  let weekStart = startOfLocalDay tz (addDays (-6) (localDayOf tz now))
  obss <- Db.observationsBetween (appDb app) weekStart now
  ov <- Db.overridesBetween (appDb app) weekStart now
  forM_ (activePets prof) $ \pet -> do
    let ins = insightsFor tz ov crs roster now pet obss
        body = boundedLines 120 (map (Narrative.observationLine tz) (filter (mentions ov roster pet) obss))
    summary <- Recap.petWeekly (appLlm app) pet (wbKind (piWellbeing ins)) (statPairs ins) body
    Db.putPetSummary (appDb app) (piId ins) now summary
  where
    onErr (e :: SomeException) =
      traceWith (ptrace app) (PetSummariesFailed (tshow e))

mentions :: Overrides -> [Pet] -> Pet -> Observation -> Bool
mentions ov roster pet o =
  not (null (subjectAppearances ov roster (KPet (petId pet)) o))

statPairs :: PetInsights -> [(Text, Text)]
statPairs ins =
  [ ("Seen", tshow (sum (piSpark ins)) <> " this week")
  , ("Days seen", tshow (length (filter (> 0) (piSpark ins))) <> " of 7")
  ]
    ++ favourite
    ++ [("Rested", if blRestPct (piBalance ins) >= 60 then "a lot" else "some")]
  where
    favourite = case piSpots ins of
      (s : _) -> [("Favourite spot", spRoom s)]
      []      -> []

-- --------------------------------------------------------------------------- --
-- Helpers
-- --------------------------------------------------------------------------- --

hasPet :: Scene -> Bool
hasPet sc = not (all (isPerson . who) (appearances sc))

concerningObs :: Observation -> Bool
concerningObs obs = case perception obs of
  Seen sc  -> wellbeing sc == Concerning || any bad (appearances sc)
  Heard sk -> isSafetySound sk
  where
    bad ap =
      let b = behaviors ap
       in not (null (concerns b)) || accidentSuspected b || injurySuspected b

-- | A UTC time as fractional POSIX seconds, the unit the event watermark and Frigate's
-- @after@/@before@ bounds use.
posixSecs :: UTCTime -> Double
posixSecs = realToFrac . utcTimeToPOSIXSeconds
