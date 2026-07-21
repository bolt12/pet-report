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
  , RunOutcome (..)
  , runOutcomeText
  , gcEffectiveWindow
  , garbageCollectNow
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar  (MVar, putMVar, tryTakeMVar)
import           Control.Exception        (SomeException, bracket, handle,
                                           throwIO, try)
import           Control.Monad            (foldM, forM_, unless, void)
import           Data.Aeson               (object, (.=))
import           Data.Aeson.Text          (encodeToLazyText)
import           Data.IORef               (IORef, atomicModifyIORef',
                                           newIORef, readIORef)
import           Data.List                (sort)
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
import           Data.Time.Zones          (utcToLocalTimeTZ)
import           System.FilePath          ((</>))
import           Text.Read                (readMaybe)

import           PetReport.App                 (App (..), appBatchLock)
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
import           PetReport.Trace               (PipelineEvent (..), Tracer,
                                                pipelineTracer, traceWith)
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

-- | The most vision-bearing items, queued frames plus Frigate events, one batch will
-- analyse. Anything past this defers to the next batch, with frames left queued and the
-- event watermark frozen, so a slow model or a burst cannot make one run unbounded and leave
-- the refresh spinner hanging for many minutes.
batchItemCap :: Int
batchItemCap = 50

-- | The wall-clock ceiling on one batch's item processing. The item cap alone does not bound
-- time: 50 items at the background budget's ~180s worst case is roughly two and a half
-- hours, all of it on the single worker thread with every queued refresh and rebuild waiting
-- behind it.
--
-- Past the deadline a slow-model run defers its remainder and records an honest "ok"
-- outcome. This generalizes the item cap, since both stop taking NEW items and route through
-- the same 'frozen' path.
batchDeadlineSecs :: NominalDiffTime
batchDeadlineSecs = 20 * 60

-- | The most events one ingest pass pulls from Frigate in a single request. Frigate returns
-- them OLDEST first, so a pass takes the oldest @limit@ after the watermark and defers the
-- rest, and a long absence catches up a page at a time. Set well above the busiest observed
-- day, so a normal batch drains in one request.
eventFetchLimit :: Int
eventFetchLimit = 500

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

-- | The per-batch processing budget: a shrinking item counter AND a wall-clock deadline.
-- Both bound how many NEW vision-bearing items one run takes on, and either being spent
-- stops the run and defers the rest through the 'frozen' path, leaving frames queued and the
-- event watermark held, for the next batch to pick up.
data Budget = Budget
  { bgLeft     :: IORef Int
  , bgDeadline :: UTCTime
  , bgClock    :: Clock.Handle
  }

-- | Build a fresh batch budget: a full item counter and a deadline
-- 'batchDeadlineSecs' out from now.
newBudget :: App -> IO Budget
newBudget app = do
  ref <- newIORef batchItemCap
  now <- Clock.now (appClock app)
  pure (Budget ref (addUTCTime batchDeadlineSecs now) (appClock app))

-- | How many item units the budget has spent, for the outcome note.
budgetUsed :: Budget -> IO Int
budgetUsed b = (batchItemCap -) <$> readIORef (bgLeft b)

-- | Take one unit of the per-batch budget. 'False' once EITHER the item counter is spent OR
-- the wall-clock deadline has passed. Checked before each item, so a large backlog is
-- deferred rather than read into memory.
takeBudget :: Budget -> IO Bool
takeBudget b = do
  now <- Clock.now (bgClock b)
  if now >= bgDeadline b
    then pure False
    else atomicModifyIORef' (bgLeft b) (\n -> if n > 0 then (n - 1, True) else (n, False))

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
  cams <- Frigate.onlineCameras (appFrigate app) (enabledCameras prof)
  now <- Clock.now (appClock app)
  let ts = round (utcTimeToPOSIXSeconds now) :: Integer
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

-- --------------------------------------------------------------------------- --
-- Batch
-- --------------------------------------------------------------------------- --

-- | Run the batch pipeline under the in-process batch mutex, so it cannot duplicate work by
-- running alongside an on-demand rebuild or an inline cleanup-now.
batch :: App -> IO ()
batch app = withBatchLock app $ do
  budget <- newBudget app
  recordingOutcome app (batchNote budget) (batchSteps app budget)
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
  budget <- newBudget app
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
    _ <- ingestWindowSafe app budget prof lo hi
    finishReportFor app prof day False

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

batchSteps :: App -> Budget -> IO ()
batchSteps app budget = do
  prof <- Db.getProfile (appDb app)
  refreshBriefStep app prof
  analyzeQueue app budget prof
  ingestEvents app budget prof
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
    forM_ (enabledCameras prof) (perCamera brief)
  where
    cfg = appConfig app
    onErr (e :: SomeException) =
      traceWith (ptrace app) (ModelUnavailable (tshow e))
    perCamera brief cam = do
      let qdir = cfgQueueDir cfg </> T.unpack cam
      files <- sort <$> listJpgs qdir
      forM_ (mapMaybe withTs files) $ \(fname, ts) -> do
        ok <- takeBudget budget
        -- Over the per-batch item cap OR past the wall-clock deadline, leave the frame
        -- queued for the next run. Checked before reading the file, so a large backlog is
        -- not all pulled into memory. The frame count on disk already bounds this path;
        -- the deadline mostly protects the unbounded event path.
        if not ok
          then pure ()
          else do
            let qpath = qdir </> fname
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
-- it, oldest-first, then persist it. A long absence catches up a page per pass, while a
-- normal batch needs a single request.
ingestEvents :: App -> Budget -> Profile -> IO ()
ingestEvents app budget prof = do
  wm <- fmap (fromMaybe 0 . (>>= parseDouble)) (Db.getState (appDb app) "last_event_ts")
  nowP <- posixSecs <$> Clock.now (appClock app)
  newWm <- ingestWindowSafe app budget prof wm nowP
  Db.setState (appDb app) "last_event_ts" (tshow newWm)

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
      let cfg = appConfig app
          labels = cfgPetLabels cfg ++ cfgAudioLabels cfg
      mevents <- Frigate.recentEvents (appFrigate app) labels lo hi eventFetchLimit
      case mevents of
        -- A failed fetch, NOT an empty window. Hold the watermark so this range is retried
        -- next pass, rather than advancing past events we never saw.
        Nothing -> pure lo
        Just events -> do
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
          pure (advanceWatermark lo frozen (length events) wm hi)
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
      | feFalsePositive ev =
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
              then pure (seen, adv wm, frozen)
              else do
                -- Only events past cooldown do real work, so only they take budget. Over
                -- the item cap OR past the wall-clock deadline, freeze the watermark so
                -- this and later events retry next pass instead of making one run
                -- unbounded. A deadline stop and a budget stop are the same thing here:
                -- both defer the remainder through the frozen watermark, leaving the events
                -- in Frigate to re-fetch.
                ok <- takeBudget budget
                if not ok
                  then pure (seen, wm, True)
                  else do
                    stored <- ingestOne app brief ev
                    if stored
                      then pure (Map.insert key (feStart ev) seen, adv wm, frozen)
                      else
                        if nowP - feStart ev > eventRetryWindowSec
                          then pure (seen, adv wm, frozen)
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
        | isJust (feEnd ev) -> pure True
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

parseDouble :: Text -> Maybe Double
parseDouble = readMaybe . T.unpack

-- | A UTC time as fractional POSIX seconds, the unit the event watermark and Frigate's
-- @after@/@before@ bounds use.
posixSecs :: UTCTime -> Double
posixSecs = realToFrac . utcTimeToPOSIXSeconds
