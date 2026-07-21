-- | The @cleanup@ resource: the schedule read (when the batch runs next), the
-- garbage-collection preview (how many moments a proposed retention window would
-- delete), and the run-now trigger.
module PetReport.Web.Cleanup
  ( cleanupInfoH
  , gcPreviewH
  , cleanupNowH
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value, object, (.=))
import           Data.Time              (addDays)
import           Servant                (Handler)

import           PetReport.App                (App (..))
import           PetReport.Config             (Config (..), hourInt)
import           PetReport.Domain.Window      (localDayOf, startOfLocalDay)
import qualified PetReport.Effect.Clock       as Clock
import qualified PetReport.Effect.Db          as Db
import           PetReport.Error              (badInput)
import qualified PetReport.Pipeline           as Pipeline
import           PetReport.Pipeline.Scheduler (nextBatchTime)

-- | How many un-kept moments GC would delete at a proposed retention window, so the settings
-- screen can warn before the owner shortens it. Counted against GC's exact deletion
-- boundary, the start of the local day @effWin@ days ago, since GC collects whole local
-- days. The preview therefore equals what the next run removes.
gcPreviewH :: App -> Maybe Int -> Handler Value
gcPreviewH _ Nothing = badInput "days is required"
gcPreviewH app (Just days) = liftIO $ do
  tz <- Clock.timeZone (appClock app)
  now <- Clock.now (appClock app)
  let effWin = Pipeline.gcEffectiveWindow days
      cutoff = startOfLocalDay tz (addDays (negate effWin) (localDayOf tz now))
  n <- Db.countCollectable (appDb app) cutoff
  pure (object ["days" .= days, "effectiveDays" .= effWin, "count" .= n])

-- | The cleanup schedule: the local hours the batch runs and when it next will, so the
-- settings screen can say so without hardcoding it.
cleanupInfoH :: App -> Handler Value
cleanupInfoH app = liftIO $ do
  tz <- Clock.timeZone (appClock app)
  now <- Clock.now (appClock app)
  let hours = cfgBatchHours (appConfig app)
  pure (object ["batchHours" .= map hourInt hours, "nextRunAt" .= nextBatchTime hours tz now])

-- | Run the cleanup now (the same garbage collection the batch does, under the same
-- lock so it cannot race a scheduled batch), returning how many un-kept moments it
-- removed, so the owner can force it and see the result. Reports @busy@ if a batch is
-- already running rather than silently doing nothing.
cleanupNowH :: App -> Handler Value
cleanupNowH app = liftIO $ do
  prof <- Db.getProfile (appDb app)
  mn <- Pipeline.garbageCollectNow app prof
  pure $ case mn of
    Just n  -> object ["collected" .= n, "busy" .= False]
    Nothing -> object ["collected" .= (0 :: Int), "busy" .= True]
