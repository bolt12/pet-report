-- | The Frigate media-retention poller. It keeps 'appRetention' fresh, so a moment's
-- clip-expiry countdown is computed on read from one source of truth. A Frigate config
-- change is picked up here and shows on the next view, with nothing stored in our database
-- and nothing to propagate. Retention is Frigate's to own; this only reads it.
module PetReport.Pipeline.Retention
  ( runRetentionPoller
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import           Control.Monad          (forever, unless, when)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T

import           PetReport.App            (App (..), appRetention)
import           PetReport.Config         (Config (..))
import qualified PetReport.Effect.Frigate as Frigate
import           PetReport.Trace          (PipelineEvent (..), pipelineTracer,
                                           traceWith)
import           PetReport.Util           (microseconds)

-- | Poll Frigate's config for its per-camera media retention on an interval,
-- publishing changes to 'appRetention'. Runs forever, under
-- 'PetReport.Web.runServer's supervision.
runRetentionPoller :: App -> IO ()
runRetentionPoller app = forever $ do
  refreshRetentionOnce app
  threadDelay (microseconds (max 1 (cfgRetentionPollSecs (appConfig app))))

-- | Fetch the current per-camera retention and, when it differs from what is published,
-- swap it in and log the change. A failed fetch yields an empty map, which reads as "no
-- update", so a transient Frigate blip cannot blank the countdown.
refreshRetentionOnce :: App -> IO ()
refreshRetentionOnce app = do
  fresh <- Frigate.mediaRetention (appFrigate app)
  unless (Map.null fresh) $ do
    cur <- readTVarIO (appRetention app)
    when (fresh /= cur) $ do
      atomically (writeTVar (appRetention app) fresh)
      traceWith
        (pipelineTracer (appTracer app))
        (RetentionUpdated (renderRetain fresh))

renderRetain :: Map Text Double -> Text
renderRetain =
  T.intercalate ", "
    . map (\(c, d) -> c <> "=" <> T.pack (show (round d :: Int)) <> "d")
    . Map.toList
