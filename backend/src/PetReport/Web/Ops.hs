-- | The operations endpoints: the model correction-rate stats, the refresh trigger
-- and its running-status poll, the free-text question agent, and the on-demand recap.
module PetReport.Web.Ops
  ( modelStatsH
  , batchH
  , resolveRefresh
  , askH
  , recapH
  , refreshH
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value, decodeStrict, object, (.=))
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time              (UTCTime, addUTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           Data.Time.Zones        (TZ)
import           Servant                (Handler)

import qualified PetReport.Analysis.Agent      as Agent
import qualified PetReport.Analysis.Narrative  as Narrative
import qualified PetReport.Analysis.IdentGuide as IdentGuide
import           PetReport.App                 (App (..), appJobs)
import           PetReport.Domain.Types        (ObsId (..))
import           PetReport.Domain.Window       (localDayOf, localDayWindow,
                                                recognizedArg,
                                                resolveDay)
import qualified PetReport.Effect.Clock        as Clock
import qualified PetReport.Effect.Db           as Db
import qualified PetReport.Effect.Llm          as Llm
import           PetReport.Error               (badInput)
import           PetReport.Pipeline.Worker     (Job (..), jobRunning, submit)
import           PetReport.Web.Common          (momentViewsByIds, nowTzProfile,
                                                orUnavailable)
import           PetReport.Web.Types           (AskReq (..), AskResp (..),
                                                RecapReq (..), RecapResp (..),
                                                RefreshReq (..), RefreshResp (..))

-- | How often owners changed the model's reading, overall and per camera, by diffing
-- raw_perception against the stored perception. The feedback signal for tuning the vision
-- prompts.
modelStatsH :: App -> Handler Value
modelStatsH app = liftIO $ do
  rows <- Db.correctionStats (appDb app)
  let total = sum [t | (_, t, _) <- rows]
      corrected = sum [x | (_, _, x) <- rows]
      rate = if total == 0 then 0 else fromIntegral corrected / fromIntegral total :: Double
  pure $
    object
      [ "total" .= total
      , "corrected" .= corrected
      , "rate" .= rate
      , "byCamera" .= [object ["camera" .= cam, "total" .= t, "corrected" .= x] | (cam, t, x) <- rows]
      ]

-- | How many surfaced moments the answer carries as cards. The agent may look at far more,
-- a whole day's timeline say, but the SPA shows a short strip of refs, so keep the most
-- recent few. A display cap, not a claim that the agent used only these.
askRefCap :: Int
askRefCap = 6

-- | Answer a question and surface the exact observations the agent looked at. The agent
-- returns the ids of the moments its tools fetched, most-recent-first, and the top few
-- enrich into cards. Deriving the refs from the question text instead would show today's
-- cards for a question about last Tuesday.
--
-- @now@, @tz@ and @prof@ are read once and handed to the agent, so it does not re-load the
-- profile just to build its prompt.
askH :: App -> AskReq -> Handler AskResp
askH app (AskReq q) = do
  (now, tz, prof) <- liftIO (nowTzProfile app)
  (ans, ids) <-
    orUnavailable app "the assistant is unavailable right now; the model may be offline" $
      Agent.ask app prof now tz q
  refs' <- liftIO (momentViewsByIds app now prof (map unObsId (take askRefCap ids)))
  pure (AskResp ans refs')
  where
    unObsId (ObsId i) = i

-- | Synthesise an on-demand narrative recap of the last @hours@ of observations, clamped to
-- 1-12. A model outage answers 503, and an empty window answers a plain "nothing to recap"
-- without calling the model at all.
recapH :: App -> RecapReq -> Handler RecapResp
recapH app (RecapReq hours) = do
  (now, tz, prof) <- liftIO (nowTzProfile app)
  let hrs = max 1 (min 12 hours)
      lo = addUTCTime (negate (fromIntegral hrs * 3600)) now
  obss <- liftIO (Db.observationsBetween (appDb app) lo now)
  guide <- liftIO (IdentGuide.identGuide (appDb app) prof)
  txt <-
    if null obss
      then pure "Nothing to recap in that window."
      else
        orUnavailable app "the recap is unavailable right now; the model may be offline" $
          Narrative.synthesize (appLlm app) Llm.interactiveBudget tz prof guide obss
  pure (RecapResp txt)

-- | Resolve the optional day of @\/api\/refresh@ to a background job. No day, or today,
-- runs the full daily batch; a valid past day builds just that day; a future or unrecognised
-- day is rejected. A @started@ response therefore only ever means a job that will run.
resolveRefresh :: TZ -> UTCTime -> Maybe Text -> Either Text Job
resolveRefresh tz now mday = case mday of
  Nothing -> Right RunBatch
  Just raw
    | not (recognizedArg raw) -> Left "invalid day"
    | otherwise ->
        let today = localDayOf tz now
            d = resolveDay tz now raw
         in if d < today
              then Right (BuildDay d)
              else if d == today then Right RunBatch else Left "day is in the future"

-- | Kick off a refresh in the background. Submitting to the worker de-duplicates, so a
-- second refresh while one is running is a no-op (@started@ is 'False'); the UI
-- polls @\/api\/batch@ to know when the run finishes. A job failure is traced by
-- the worker, never lost.
refreshH :: App -> RefreshReq -> Handler RefreshResp
refreshH app (RefreshReq mday) = do
  now <- liftIO (Clock.now (appClock app))
  tz <- liftIO (Clock.timeZone (appClock app))
  case resolveRefresh tz now mday of
    Left msg  -> badInput msg
    Right job -> liftIO (RefreshResp <$> submit (appJobs app) job)

-- | Whether the refresh job for a given day is currently running, for that day's spinner.
-- The optional @day@ is resolved through the same rule as @\/api\/refresh@, so a day polls
-- exactly the job its refresh submits (today/none -> the daily batch, a past day -> that
-- day's build); an unrecognised or future day is never "running". @last@ is the most
-- recent run's global outcome, so the client can tell an @ok@ run from a @skipped@ one.
batchH :: App -> Maybe Text -> Handler Value
batchH app mday = liftIO $ do
  now <- Clock.now (appClock app)
  tz <- Clock.timeZone (appClock app)
  running <- case resolveRefresh tz now mday of
    Right job -> jobRunning (appJobs app) job
    Left _    -> pure False
  mlast <- Db.getState (appDb app) "last_batch"
  caught <- caughtUpOn app tz now mday
  let lastVal = mlast >>= (decodeStrict . encodeUtf8) :: Maybe Value
  pure (object ["running" .= running, "last" .= lastVal, "caughtUp" .= caught])

-- | Whether every event of the day in question has been looked at yet.
--
-- Two ways a day can be finished, and both have to count, because the button this notice
-- offers takes the second route. A past day is rebuilt by 'Pipeline.buildDay', which sweeps
-- that day's own window and leaves the global watermark alone, so rebuilding an old day
-- cannot rewind steady-state ingest. Asking the watermark alone would therefore keep the
-- notice up on a day the owner had just finished, no matter how often they pressed it.
--
-- A finished day (its end is behind us) is caught up once the watermark has passed that end.
-- The test is against the day's END, never "now": a caught-up system's watermark is
-- legitimately hours old between batches, so comparing with now would report a finished day
-- as behind twice a day, every day, and the notice would be noise within a week.
--
-- An open day, today, cannot be judged that way: its end is in the future, so the watermark
-- can never reach it, and comparing with "now" instead would flag today as behind for the
-- whole gap between batches, since the watermark only touches the present for an instant when
-- a batch runs. What actually matters is whether outstanding work is piling up, so an open day
-- reads the drained flag: the last ingest either reached its window's end (nothing outstanding)
-- or parked on a backlog the owner can act on. A normal between-batch lag is not falling behind.
--
-- No watermark at all reads as caught up: a fresh install has nothing outstanding, and saying
-- otherwise on first run would be a lie the owner cannot act on.
caughtUpOn :: App -> TZ -> UTCTime -> Maybe Text -> IO Bool
caughtUpOn app tz now mday = do
  -- Resolved exactly as 'resolveRefresh' resolves it, so the notice and the refresh button
  -- can never disagree about which day they mean.
  let today = localDayOf tz now
      day = case mday of
        Just raw | recognizedArg raw -> resolveDay tz now raw
        _                            -> today
  swept <- Db.getDaySwept (appDb app) day
  if swept
    then pure True
    else do
      stored <- Db.getIngestWatermark (appDb app)
      case stored of
        Nothing -> pure True
        Just secs ->
          -- An open day (today, or later) is judged by the drained flag; a finished day, by
          -- whether the watermark has passed its end. localDayWindow already clamps a day's
          -- end to now, so the split is on the calendar day, not that clamped bound.
          if day >= today
            then Db.getIngestDrained (appDb app)
            else do
              let (_, dayEnd) = localDayWindow tz now day
                  wm = posixSecondsToUTCTime (realToFrac secs)
              pure (wm >= dayEnd)
