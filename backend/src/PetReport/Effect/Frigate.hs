{-# LANGUAGE NoMonomorphismRestriction #-}

-- The query-option builders in 'withHandle' must stay scheme-polymorphic so they combine
-- with either an http or an https base, which is why the monomorphism restriction is off
-- here. The request plumbing itself lives in "PetReport.Effect.Http".

-- | The Frigate NVR effect: read-only REST, as a record-of-functions handle. Online-camera
-- status is cached briefly so home-page renders do not hammer Frigate. Every call degrades
-- rather than throwing, yielding 'Nothing' or an empty list on a fetch failure so nothing
-- escapes into the pipeline.
module PetReport.Effect.Frigate
  ( Handle (..)
  , withHandle
  , FrigateEvent (..)
  , MediaKind (..)
  , eventMedia
  , TranscribeError (..)
  , FrigateConfig (..)
  , FrigateStats (..)
  , Transcript (..)
  ) where

import           Control.Applicative    ((<|>))
import           Control.Concurrent.STM (STM, atomically)
import           Control.Exception      (try)
import           Data.Aeson             (FromJSON (..), Object, Value, object,
                                         withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KeyMap
import           Data.Aeson.Types       (Parser)
import           Data.ByteString        (ByteString)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, diffUTCTime, getCurrentTime)
import           Network.HTTP.Req       (HttpException, (=:))

import           PetReport.Domain.Types (BaseUrl, baseUrlText)
import qualified PetReport.Effect.Http  as Http
import           PetReport.Util         (nonBlank)

-- | A Frigate event, mapped onto the fields of the API's @EventResponse@ we use.
data FrigateEvent = FrigateEvent
  { feId            :: Text
  , feCamera        :: Text
  , feLabel         :: Text
  , feScore         :: Double
  , feStart         :: Double
  , feEnd           :: Maybe Double
  -- ^ End time, or 'Nothing' while the event is still in progress. This is how ingest
  -- tells a genuinely media-less COMPLETED event, which it advances past, from a still-open
  -- one whose snapshot may yet arrive, which it holds and retries.
  , feFalsePositive :: Bool
  -- ^ Frigate-confirmed non-detection; ingesting one would store a bogus sighting.
  , feHasClip       :: Bool
  , feHasSnapshot   :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON FrigateEvent where
  parseJSON = withObject "FrigateEvent" $ \o -> do
    feId <- o .: "id"
    feCamera <- o .: "camera"
    feLabel <- o .: "label"
    feStart <- o .:? "start_time" .!= 0
    feEnd <- o .:? "end_time"
    feFalsePositive <- o .:? "false_positive" .!= False
    feHasClip <- o .:? "has_clip" .!= False
    feHasSnapshot <- o .:? "has_snapshot" .!= False
    -- The tracked-object score lives under @data@, since EventResponse has no top-level
    -- score. Prefer top_score, falling back to score for in-progress events without it.
    d <- o .:? "data" .!= (mempty :: Object)
    ts <- d .:? "top_score"
    sc <- d .:? "score"
    let feScore = fromMaybe 0 (ts <|> sc)
    pure FrigateEvent {..}

-- | What an event actually has to analyse or play. A clip event also carries a snapshot; a
-- 'NoMedia' event has nothing.
data MediaKind = HasClip | SnapshotOnly | NoMedia
  deriving stock (Eq, Show)

-- | Resolve an event's media from its flags, once, so consumers never fetch a snapshot
-- Frigate has already said is absent.
eventMedia :: FrigateEvent -> MediaKind
eventMedia ev
  | feHasClip ev     = HasClip
  | feHasSnapshot ev = SnapshotOnly
  | otherwise        = NoMedia

-- | Why a transcription attempt produced no text: Frigate refused the trigger (it needs
-- an admin role, or transcription is off) versus a successful trigger that yielded none.
data TranscribeError = TranscribeRefused | NoTranscript
  deriving stock (Eq, Show)

-- | The slice of Frigate's @\/api\/config@ this effect reads. Frigate cannot run without a
-- @cameras@ object, so its absence is a decode failure. @audio_transcription@ is optional,
-- and absent means off.
data FrigateConfig = FrigateConfig
  { fcCameraNames    :: [Text]
  -- ^ The KEYS of @config.cameras@, i.e. every camera name Frigate has configured.
  , fcTranscription  :: Bool
  -- ^ @config.audio_transcription.enabled@; absent (either key) reads as 'False'.
  , fcMediaRetain    :: Map Text Double
  -- ^ Per-camera media retention in days: how long Frigate keeps a camera's event footage,
  -- and what drives the clip-expiry countdown. Taken as the max of
  -- @snapshots.retain.default@, @record.alerts.retain.days@ and
  -- @record.detections.retain.days@, which upper-bounds when the media is fully gone. Read
  -- from each already-merged camera object, falling back to the top-level value.
  }
  deriving stock (Eq, Show)

instance FromJSON FrigateConfig where
  parseJSON = withObject "FrigateConfig" $ \o -> do
    cs <- o .: "cameras"
    let fcCameraNames = map Key.toText (KeyMap.keys (cs :: Object))
    at <- o .:? "audio_transcription" .!= (mempty :: Object)
    fcTranscription <- at .:? "enabled" .!= False
    gCands <- retainCandidates o
    let gFallback = if null gCands then defaultMediaRetain else maximum gCands
    perCam <- traverse (camRetain gFallback) (KeyMap.toList (cs :: Object))
    let fcMediaRetain = Map.fromList perCam
    pure FrigateConfig {..}

-- | Frigate's documented default snapshot retention, used only when a config exposes no
-- retention at all. The countdown then degrades to a sane number rather than vanishing.
defaultMediaRetain :: Double
defaultMediaRetain = 10

-- | The retention (days) of one already-merged camera object: the max of its
-- retain candidates, or the global fallback when it declares none.
camRetain :: Double -> (Key.Key, Value) -> Parser (Text, Double)
camRetain fallback (k, v) = do
  cands <- withObject "camera" retainCandidates v
  pure (Key.toText k, if null cands then fallback else maximum cands)

-- | The retain-days values present under an object: snapshots and both record classes. A
-- missing key contributes nothing to the list.
retainCandidates :: Object -> Parser [Double]
retainCandidates o =
  catMaybes
    <$> sequence
      [ dig ["snapshots", "retain", "default"] o
      , dig ["record", "alerts", "retain", "days"] o
      , dig ["record", "detections", "retain", "days"] o
      ]

-- | Follow a key path to a numeric leaf, tolerating a missing OR wrong-typed link. The
-- @\<|> pure Nothing@ stops an unexpected retain shape from failing the whole
-- 'FrigateConfig' decode, which also backs camera discovery and transcription detection. A
-- surprising retention value blanks the countdown, not the roster.
dig :: [Text] -> Object -> Parser (Maybe Double)
dig path o = go path o <|> pure Nothing
  where
    go [] _ = pure Nothing
    go [k] obj = obj .:? Key.fromText k
    go (k : ks) obj = do
      mchild <- obj .:? Key.fromText k
      maybe (pure Nothing) (go ks) mchild

-- | The slice of Frigate's @\/api\/stats@ this effect reads: per-camera frame rate, keyed
-- by camera name. A camera object with no @camera_fps@ contributes 0.
newtype FrigateStats = FrigateStats {fsCameraFps :: Map Text Double}
  deriving stock (Eq, Show)

instance FromJSON FrigateStats where
  parseJSON = withObject "FrigateStats" $ \o -> do
    cs <- o .:? "cameras" .!= (mempty :: Object)
    fps <- traverse fpsOf (KeyMap.toList cs)
    pure (FrigateStats (Map.fromList fps))
    where
      fpsOf (k, v) = do
        cam <- parseJSON v
        f <- cam .:? "camera_fps" .!= 0
        pure (Key.toText k, f)

-- | A transcript pulled from a Frigate response, tolerating the shapes its various versions
-- use: a top-level @transcription@, @text@ or @transcript@, or one nested under @data@. A
-- blank or whitespace-only result decodes as 'Nothing'.
newtype Transcript = Transcript {unTranscript :: Maybe Text}
  deriving stock (Eq, Show)

instance FromJSON Transcript where
  parseJSON = withObject "Transcript" $ \o -> do
    d <- o .:? "data" .!= (mempty :: Object)
    dt <- d .:? "transcription"
    dx <- d .:? "text"
    t1 <- o .:? "transcription"
    t2 <- o .:? "text"
    t3 <- o .:? "transcript"
    let raw = dt <|> dx <|> t1 <|> t2 <|> t3
    pure (Transcript (nonBlank =<< raw))

-- | The Frigate client: a record of read-only NVR queries. Each degrades to
-- 'Nothing' or an empty result on a fetch failure rather than throwing into the pipeline.
data Handle = Handle
  { onlineCameras :: [Text] -> IO [Text]
  -- ^ Of the given cameras, those Frigate reports online. The list is passed per call
  -- rather than baked in at construction, so the long-lived @serve@ process honours a
  -- freshly-configured camera set.
  , listCameras   :: IO [Text]
  -- ^ Every camera Frigate has configured, for setup auto-discovery, so the owner never
  -- types a camera name by hand.
  , latestFrame   :: Text -> IO (Maybe ByteString)
  , recentEvents  :: [Text] -> Double -> Double -> Int -> IO (Maybe [FrigateEvent])
  -- ^ Events in the half-open start-time window @[after, before)@, OLDEST first, up to
  -- @limit@. 'Nothing' means the fetch FAILED, as distinct from an empty window
  -- (@Just []@), and ingest holds its watermark on 'Nothing' rather than advancing past
  -- events it never saw.
  --
  -- Oldest-first is what lets the watermark advance contiguously: a pass takes the oldest
  -- @limit@ after it and defers the rest, never skipping an older event.
  , eventSnapshot :: Text -> IO (Maybe ByteString)
  , eventClip     :: Text -> IO (Maybe ByteString)
  , transcribe    :: Text -> IO (Either TranscribeError Text)
  -- ^ Trigger and read a speech event's transcript. Frigate's @PUT /audio/transcribe@ is
  -- only a trigger, its body being @{success,message}@ and never the text, and it needs an
  -- admin role. A refusal, such as a 403 for the app's viewer role, is
  -- 'TranscribeRefused'. The text is then read from the event record, or 'NoTranscript' if
  -- none came back.
  , transcriptionEnabled :: IO Bool
  -- ^ Whether Frigate has speech transcription turned on, so the API can tell an owner the
  -- feature is off instead of failing a transcribe with an opaque error. 'False' on any
  -- config-read failure.
  , mediaRetention       :: IO (Map Text Double)
  -- ^ Per-camera media retention in days, read from Frigate's config for the clip-expiry
  -- countdown. Empty on any config-read failure, in which case the countdown does not show
  -- rather than guessing.
  }

-- | @withHandle getBase@ opens a read-only Frigate client that resolves its base URL fresh
-- from @getBase@ on every call, so a setup change needs no restart.
withHandle :: STM BaseUrl -> (Handle -> IO a) -> IO a
withHandle getBaseStm k = do
  cache <- newIORef Nothing
  let getBase = baseUrlText <$> atomically getBaseStm
  k
    Handle
      { onlineCameras = \cams -> do
          base <- getBase
          cachedOnline base cams cache
      , listCameras = maybe [] fcCameraNames <$> (getBase >>= getConfig)
      , transcriptionEnabled = maybe False fcTranscription <$> (getBase >>= getConfig)
      , mediaRetention = maybe mempty fcMediaRetain <$> (getBase >>= getConfig)
      , latestFrame = \cam -> do
          base <- getBase
          hush <$> tryHttp (Http.getBytes "Frigate" base ["api", cam, "latest.jpg"] latestOpts)
      , recentEvents = \labels after before limit -> do
          base <- getBase
          hush <$> tryHttp (Http.get "Frigate" base ["api", "events"] (eventsOpts labels after before limit))
      , eventSnapshot = \eid -> do
          base <- getBase
          hush <$> tryHttp (Http.getBytes "Frigate" base ["api", "events", eid, "snapshot.jpg"] snapOpts)
      , eventClip = \eid -> do
          base <- getBase
          hush <$> tryHttp (Http.getBytes "Frigate" base ["api", "events", eid, "clip.mp4"] mempty)
      , transcribe = \eid -> do
          base <- getBase
          -- The PUT only triggers transcription, its body being {success,message} rather
          -- than the text, and it needs an admin role, so treat any failure as a refusal.
          -- On success the transcript comes from the event record, under `data`.
          putRes <- tryHttp (Http.put "Frigate" base ["api", "audio", "transcribe"] (object ["event_id" .= eid]) mempty :: IO Value)
          case putRes of
            Left _ -> pure (Left TranscribeRefused)
            Right _ -> do
              evRes <- tryHttp (Http.get "Frigate" base ["api", "events", eid] mempty :: IO Transcript)
              pure (maybe (Left NoTranscript) Right (unTranscript =<< hush evRes))
      }
  where
    latestOpts =
      "bbox" =: (0 :: Int)
        <> "timestamp" =: (0 :: Int)
        <> "motion" =: (0 :: Int)
        <> "regions" =: (0 :: Int)
    snapOpts = "bbox" =: (0 :: Int) <> "timestamp" =: (0 :: Int)
    eventsOpts labels after before limit =
      "labels" =: T.intercalate "," labels
        <> "after" =: (after :: Double)
        <> "before" =: (before :: Double)
        <> "limit" =: (limit :: Int)
        -- Ask Frigate for the OLDEST first so ingest advances its watermark
        -- contiguously (see the 'recentEvents' note); Frigate defaults to newest-first.
        -- Deliberately NOT filtered by has_snapshot/in_progress: has_snapshot=1 would drop
        -- audio events (they carry no snapshot) and in_progress=0 would let the watermark
        -- advance past a still-open event and never re-fetch it. Both are handled per-event
        -- instead (audio via soundObs, media/progress via eventMedia + feEnd in ingestOne).
        <> "sort" =: ("date_asc" :: Text)

-- | Online cameras among @cams@, from a ~15s cache of Frigate's @stats@; on a
-- fetch failure the last good stats are reused. Caching the decoded stats (rather
-- than a filtered list) lets each call check a different, freshly-configured
-- camera list without invalidating the cache.
cachedOnline :: Text -> [Text] -> IORef (Maybe (UTCTime, FrigateStats)) -> IO [Text]
cachedOnline base cams cache = do
  now <- getCurrentTime
  cached <- readIORef cache
  stats <- case cached of
    Just (t, s) | diffUTCTime now t < 15 -> pure (Just s)
    _ -> do
      r <- tryHttp (Http.get "Frigate" base ["api", "stats"] mempty)
      case r of
        Right s -> writeIORef cache (Just (now, s)) >> pure (Just s)
        Left _  -> pure (snd <$> cached)
  pure (maybe [] (onlineFrom cams) stats)

onlineFrom :: [Text] -> FrigateStats -> [Text]
onlineFrom cams (FrigateStats fps) =
  filter (\c -> fromMaybe 0 (Map.lookup c fps) > 0) cams

-- | Fetch and decode Frigate's @/api/config@, or 'Nothing' on any failure. The three
-- config-derived accessors share this rather than each re-spelling the fetch/decode.
getConfig :: Text -> IO (Maybe FrigateConfig)
getConfig base = hush <$> tryHttp (Http.get "Frigate" base ["api", "config"] mempty)

-- | Catch a failed request (a connection error, timeout, or non-2xx status all
-- surface as an 'HttpException'), so a call degrades to its empty default. A
-- malformed base URL is a config error, not a transient fault: the shared
-- 'Http.get'/'Http.getBytes'/'Http.put' throw an 'IOException' for it, which is
-- deliberately NOT caught here so it surfaces (as a pipeline warning, or a visible
-- capture failure) rather than silently reading empty.
tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = try

-- | Collapse a caught request to a 'Maybe', for the callers that do not care why a
-- fetch failed, only that it did.
hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
