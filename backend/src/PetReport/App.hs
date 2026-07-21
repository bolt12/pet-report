-- | The application environment: the config plus every effect handle, opened
-- once by 'withApp' and shared by the capture, batch, and serve workflows.
module PetReport.App
  ( App (..)
  , Runtime (..)
  , EffSettings (..)
  , appSettings
  , appJobs
  , appRetention
  , appBatchLock
  , withApp
  , refreshSettings
  ) where

import           Control.Concurrent.MVar   (MVar, newMVar)
import           Control.Concurrent.STM    (TVar, atomically, newTVarIO,
                                            readTVar, writeTVar)
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           PetReport.Config          (Config (..), loadConfig, orElse)
import           PetReport.Domain.Profile  (Profile (..), enabledCameras)
import           PetReport.Domain.Types    (BaseUrl (..), Camera (..),
                                            ModelName (..))
import qualified PetReport.Effect.Clock    as Clock
import qualified PetReport.Effect.Db       as Db
import qualified PetReport.Effect.Ffmpeg   as Ffmpeg
import qualified PetReport.Effect.Frigate  as Frigate
import qualified PetReport.Effect.Llm      as Llm
import qualified PetReport.Effect.Ntfy     as Ntfy
import qualified LLM.Trace                 as LlmTrace
import           PetReport.Trace           (StartupEvent (..), Trace, Tracer,
                                            dbTracer, llmTracer, logLevelFromEnv,
                                            ntfyTracer, renderingTracerAt,
                                            startupTracer, traceWith)
import           PetReport.Pipeline.Worker (Jobs, newJobs)
import           System.Exit               (exitFailure)

-- | The connection settings a setup change can alter at runtime. Held in a 'TVar' so the
-- long-lived clock, Frigate, and Llm handles pick up an edit with no restart.
data EffSettings = EffSettings
  { efFrigate :: BaseUrl
  , efLlm     :: BaseUrl
  , efModel   :: ModelName
  , efTz      :: Text
  -- ^ The effective IANA zone name, re-read by the clock on every call.
  }

-- | Everything a workflow needs, opened once by 'withApp'. Every field is fixed for the
-- process's life EXCEPT 'appRuntime', which holds the shared mutable state. Both config
-- fields are startup snapshots; the live-editable values sit in 'appRuntime', not here.
data App = App
  { appConfig     :: Config
  -- ^ The effective config resolved at startup (env overlaid by the stored profile).
  -- Its connection settings are superseded at runtime by 'appSettings'; the rest
  -- (paths, batch hours, capture interval) are read straight from here.
  , appBaseConfig :: Config
  -- ^ The env-derived base, kept so a profile change recomputes effective settings
  -- against the env fallback rather than the previous profile value.
  , appClock      :: Clock.Handle
  , appDb         :: Db.Handle
  , appLlm        :: Llm.Handle
  , appFrigate    :: Frigate.Handle
  , appFfmpeg     :: Ffmpeg.Handle
  , appNtfy       :: Ntfy.Handle
  , appTracer     :: Tracer IO Trace
  , appRuntime    :: Runtime
  -- ^ The shared mutable state. Reach into it through the 'appSettings' \/ 'appJobs' \/
  -- 'appRetention' \/ 'appBatchLock' accessors rather than the 'rt*' fields directly.
  }

-- | The process's shared mutable state, held apart from the immutable environment so the
-- boundary shows up in the type. Every 'App' field outside 'appRuntime' is fixed for the
-- process's life; these four are cells that change as it runs.
data Runtime = Runtime
  { rtSettings  :: TVar EffSettings
  -- ^ The live connection settings a setup change alters at runtime.
  , rtJobs      :: Jobs
  -- ^ The serve process's background-work queue (batch, past-day rebuild, brief regen).
  -- Handlers submit to it instead of forking; one worker drains it, so a second refresh
  -- cannot overlap the first and the UI can poll for progress.
  , rtRetention :: TVar (Map Text Double)
  -- ^ Frigate's per-camera media retention (days), refreshed by the retention poller.
  -- A moment's clip-expiry countdown is computed from this on read, so a Frigate config
  -- change shows up on the next view with nothing to store. Empty until the first poll.
  , rtBatchLock :: MVar ()
  -- ^ In-process mutex serialising the batch, buildDay and cleanup-now mutations across
  -- the job worker and the inline cleanup handler. Full when free, so a would-be second
  -- run finds it taken and skips rather than racing the GC and rollup writes.
  }

appSettings :: App -> TVar EffSettings
appSettings = rtSettings . appRuntime

appJobs :: App -> Jobs
appJobs = rtJobs . appRuntime

appRetention :: App -> TVar (Map Text Double)
appRetention = rtRetention . appRuntime

appBatchLock :: App -> MVar ()
appBatchLock = rtBatchLock . appRuntime

-- | Load config, open the database, and let the stored profile override the
-- infrastructure URLs and camera list (env values are the fallback default), then
-- open the remaining handles from the effective config and run.
withApp :: (App -> IO a) -> IO a
withApp k = do
  (minLevel, badLevel) <- logLevelFromEnv
  let tracer = renderingTracerAt minLevel
  -- Traced here rather than inside 'logLevelFromEnv', because it has to go through the
  -- tracer the bad value was meant to configure. Warning-level, so it still shows at the
  -- Info default it fell back to.
  mapM_ (traceWith (startupTracer tracer) . UnknownLogLevel) badLevel
  cfg0 <-
    loadConfig >>= \case
      Left errs -> mapM_ (traceWith (startupTracer tracer) . ConfigInvalid) errs >> exitFailure
      Right c -> pure c
  jobs <- newJobs
  retentionVar <- newTVarIO mempty
  batchLockVar <- newMVar ()
  -- Open the database first so the stored profile can steer the effective URLs
  -- and timezone before any zone-dependent handle (the clock) is built.
  Db.withHandle (dbTracer tracer) (cfgDbPath cfg0) $ \db -> do
    prof <- Db.getProfile db
    let cfg = applyProfile prof cfg0
    urlsVar <- newTVarIO (effSettingsOf cfg)
    Clock.withHandle (efTz <$> readTVar urlsVar) $ \clock ->
      Llm.withHandle ((\u -> (efLlm u, efModel u)) <$> readTVar urlsVar) $ \llm ->
        Frigate.withHandle (efFrigate <$> readTVar urlsVar) $ \frigate ->
          Ffmpeg.withHandle $ \ffmpeg ->
            Ntfy.withHandle (ntfyTracer tracer) (cfgNtfyUrl cfg) (cfgPublicUrl cfg) $ \ntfy ->
              k
                App
                  { appConfig = cfg
                  , appBaseConfig = cfg0
                  , appClock = clock
                  , appDb = db
                  , appLlm = LlmTrace.traced (traceWith (llmTracer tracer)) llm
                  , appFrigate = frigate
                  , appFfmpeg = ffmpeg
                  , appNtfy = ntfy
                  , appTracer = tracer
                  , appRuntime =
                      Runtime
                        { rtSettings = urlsVar
                        , rtJobs = jobs
                        , rtRetention = retentionVar
                        , rtBatchLock = batchLockVar
                        }
                  }

-- | Recompute the effective connection settings from a freshly saved profile and the env
-- base, then publish them to the shared 'TVar'. The long-lived handles pick them up on
-- their next call.
refreshSettings :: App -> Profile -> IO ()
refreshSettings app prof =
  let cfg = applyProfile prof (appBaseConfig app)
   in atomically $ writeTVar (appSettings app) (effSettingsOf cfg)

-- | The effective connection settings a 'Config' resolves to. Built with field labels so
-- the two 'BaseUrl' fields can't be swapped.
effSettingsOf :: Config -> EffSettings
effSettingsOf cfg =
  EffSettings
    { efFrigate = BaseUrl (cfgFrigateUrl cfg)
    , efLlm = BaseUrl (cfgLlamaUrl cfg)
    , efModel = ModelName (cfgVisionModel cfg)
    , efTz = cfgTimeZone cfg
    }

-- | Overlay the profile's connection settings onto the env-derived config. Blank values
-- fall back to the env default and an empty camera list keeps the env list, so a fresh
-- unconfigured profile changes nothing.
applyProfile :: Profile -> Config -> Config
applyProfile prof cfg =
  cfg
    { cfgFrigateUrl = frigateUrl prof `orElse` cfgFrigateUrl cfg
    , cfgLlamaUrl = modelUrl prof `orElse` cfgLlamaUrl cfg
    , cfgVisionModel = visionModel prof `orElse` cfgVisionModel cfg
    , cfgTimeZone = timeZone prof `orElse` cfgTimeZone cfg
    , cfgCameras = case filter (not . T.null) (enabledCameras prof) of
        []   -> cfgCameras cfg
        cams -> map Camera cams
    }
