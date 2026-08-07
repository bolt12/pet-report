-- | The serve process's periodic work, run as background threads rather than external
-- timers, so all of pet-report's automation lives in the one process alongside the job
-- worker and the retention poller. Both loops run forever, supervised by
-- 'PetReport.Web.runServer'.
module PetReport.Pipeline.Scheduler
  ( runCaptureScheduler
  , runBatchScheduler
  , nextBatchTime
  ) where

import           Control.Concurrent  (threadDelay)
import           Control.Exception   (SomeException, handle)
import           Control.Monad       (forever, void)
import           Data.List           (sort)
import           Data.Maybe          (listToMaybe)
import           Data.Text           (Text)
import           Data.Time           (NominalDiffTime, UTCTime, addDays,
                                      diffUTCTime)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import           Data.Time.Zones     (TZ, localTimeToUTCTZ)

import           PetReport.App             (App (..), appJobs, applyProfile)
import           PetReport.Config          (Config (..), Hour, hourInt)
import           PetReport.Domain.Window   (localDayOf)
import qualified PetReport.Effect.Clock    as Clock
import qualified PetReport.Effect.Db       as Db
import qualified PetReport.Pipeline        as Pipeline
import           PetReport.Util            (microseconds, tshow)
import           PetReport.Trace           (PipelineEvent (..), pipelineTracer,
                                            traceWith)
import           PetReport.Pipeline.Worker (Job (RunBatch), submit)

-- | Queue a frame per online camera on an interval. Each pass is guarded, so a camera
-- offline or a Frigate blip logs and the loop carries on.
runCaptureScheduler :: App -> IO ()
runCaptureScheduler app = forever $ do
  guarded "capture" app (Pipeline.capture app)
  threadDelay . microseconds . max 60 =<< captureInterval app

-- | The interval between capture passes, re-read each time round rather than closed over.
--
-- 'appConfig' is frozen when the process starts, so reading it here would make the in-app
-- setting need a restart, when every other in-app setting takes effect without one. Going
-- through 'applyProfile' keeps the env fallback in one place instead of restating it.
--
-- A profile that cannot be read falls back to the env value: a database blip should slow
-- nothing down, and must not kill the loop and stop capture silently.
captureInterval :: App -> IO NominalDiffTime
captureInterval app =
  handle (\(_ :: SomeException) -> pure (cfgCaptureSecs (appConfig app))) $ do
    prof <- Db.getProfile (appDb app)
    pure (cfgCaptureSecs (applyProfile prof (appBaseConfig app)))

-- | At each configured local hour, submit a batch to the worker, which de-duplicates it
-- against any on-demand refresh so the two cannot overlap, then sleep until the next hour.
runBatchScheduler :: App -> IO ()
runBatchScheduler app = forever $ do
  delay <- untilNextBatch app
  threadDelay delay
  guarded "batch" app (void (submit (appJobs app) RunBatch))

-- | The next configured batch time strictly after @now@, in the owner's timezone, or
-- 'Nothing' if no hours are configured. Pure, so a handler can report the next run.
nextBatchTime :: [Hour] -> TZ -> UTCTime -> Maybe UTCTime
nextBatchTime hours tz now =
  let today = localDayOf tz now
      atHour d h = localTimeToUTCTZ tz (LocalTime d (TimeOfDay (hourInt h) 0 0))
   in listToMaybe (sort [atHour d h | d <- [today, addDays 1 today], h <- hours, atHour d h > now])

-- | Microseconds until the next configured batch hour (re-check in an hour if none).
untilNextBatch :: App -> IO Int
untilNextBatch app = do
  tz <- Clock.timeZone (appClock app)
  now <- Clock.now (appClock app)
  pure $ case nextBatchTime (cfgBatchHours (appConfig app)) tz now of
    Just t  -> microseconds (max 1 (diffUTCTime t now))
    Nothing -> microseconds 3600

guarded :: Text -> App -> IO () -> IO ()
guarded what app =
  handle (\e -> traceWith (pipelineTracer (appTracer app)) (SchedulerStepFailed what (tshow (e :: SomeException))))
