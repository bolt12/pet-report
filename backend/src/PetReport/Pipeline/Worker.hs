-- | The serve process's single background worker. Request handlers submit 'Job's over STM
-- instead of spawning threads, and one long-lived worker, supervised by
-- 'PetReport.Web.runServer', drains them one at a time.
--
-- A @'TVar' ('Set' 'Job')@ of in-flight keys makes overlap impossible and de-duplication
-- free: a burst of refreshes collapses to one run, and the same past day rebuilds at most
-- once while queued or running.
module PetReport.Pipeline.Worker
  ( Job (..)
  , Jobs
  , newJobs
  , submit
  , jobRunning
  , runJobs
  ) where

import           Control.Concurrent.STM (TQueue, TVar, atomically, modifyTVar',
                                         newTQueueIO, newTVarIO, readTQueue,
                                         readTVar, writeTQueue)
import           Control.Exception      (finally)
import           Control.Monad          (forever)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (Day)
import           PetReport.Util         (catchSync)
import           PetReport.Trace        (PipelineEvent (..), Tracer, traceWith)

-- | The background work a serve handler can request. The only payload is the target 'Day',
-- which keeps 'Job' 'Ord' for the de-duplicating 'Set' and keeps this a leaf module. A
-- @Profile@ payload would pull in the domain and has no 'Ord' anyway; the 'RefreshBrief'
-- handler reads the latest profile from the database itself.
data Job
  = RunBatch      -- ^ the full daily batch for today (analysis, prune, report, push)
  | BuildDay !Day -- ^ ingest + synthesise one day's report (may be its first build)
  | RefreshBrief  -- ^ regenerate the cached identification brief
  deriving stock (Eq, Ord, Show)

-- | The worker's shared state: the pending job queue, plus the queued-or-running key set
-- that 'submit' de-duplicates against and 'jobRunning' polls. One per serve process.
data Jobs = Jobs
  { jQueue    :: !(TQueue Job)
  , jInflight :: !(TVar (Set Job))
  }

newJobs :: IO Jobs
newJobs = Jobs <$> newTQueueIO <*> newTVarIO Set.empty

-- | Enqueue a job, de-duplicating against anything queued or running. 'True' when it was
-- newly enqueued, 'False' when an identical job was already in flight, which makes a second
-- refresh during a run a no-op.
submit :: Jobs -> Job -> IO Bool
submit jobs job = atomically $ do
  inflight <- readTVar (jInflight jobs)
  if Set.member job inflight
    then pure False
    else do
      modifyTVar' (jInflight jobs) (Set.insert job)
      writeTQueue (jQueue jobs) job
      pure True

-- | Whether a specific job is in flight, for the per-day refresh spinner. Matched exactly
-- against the in-flight set, so each day polls its own job: today's refresh checks
-- 'RunBatch', a past-day rebuild checks its own @'BuildDay' d@, and the two never cross.
jobRunning :: Jobs -> Job -> IO Bool
jobRunning jobs job = atomically (Set.member job <$> readTVar (jInflight jobs))

-- | The worker loop: block on the queue, run one job under a guard, clear its
-- in-flight key. Runs forever.
runJobs :: Jobs -> Tracer IO PipelineEvent -> (Job -> IO ()) -> IO ()
runJobs jobs tr perform = forever $ do
  job <- atomically (readTQueue (jQueue jobs))
  guarded tr (label job) (perform job)
    `finally` atomically (modifyTVar' (jInflight jobs) (Set.delete job))

-- | Run one job, tracing a synchronous failure and carrying on, so one bad job never kills
-- the worker. An asynchronous exception, notably the 'AsyncCancelled' that 'withAsync'
-- raises on shutdown, is re-thrown so the worker can still be cancelled cleanly.
guarded :: Tracer IO PipelineEvent -> Text -> IO () -> IO ()
guarded tr lbl act =
  act `catchSync` \e -> traceWith tr (JobFailed lbl (T.pack (show e)))

label :: Job -> Text
label RunBatch     = "batch"
label (BuildDay d) = "build " <> T.pack (show d)
label RefreshBrief = "brief refresh"
