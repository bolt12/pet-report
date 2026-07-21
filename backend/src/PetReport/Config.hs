-- | Runtime configuration, sourced from environment variables (set by whatever
-- launches the service). Only infrastructure lives here: the pet roster and report
-- preferences are user-owned app state persisted in the database, not config.
module PetReport.Config
  ( Config (..)
  , Hour
  , mkHour
  , hourInt
  , Port
  , mkPort
  , portNumber
  , loadConfig
  , parseListen
  , orElse
  ) where

import           Data.Maybe          (fromMaybe, isNothing, mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (encodeUtf8)
import           Data.Time.Clock     (NominalDiffTime)
import           Data.Time.Zones.All (tzByName)
import           Data.Word           (Word16)
import           System.Directory    (XdgDirectory (XdgData), getXdgDirectory)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((</>))
import           Text.Read           (readMaybe)

import           PetReport.Domain.Types (Camera (..))
import           PetReport.Effect.Http  (resolvesToHttp)
import           PetReport.Util         (nonBlank)

-- | The trimmed, non-empty text of the 'Maybe', or the default. Lets a profile value
-- override an env default only when it is actually set.
orElse :: Maybe Text -> Text -> Text
orElse m def = case m of
  Just t | not (T.null (T.strip t)) -> t
  _                                 -> def

-- | An hour of the day, @0..23@ by construction, so a schedule can never build an
-- invalid 'Data.Time.LocalTime.TimeOfDay'.
newtype Hour = Hour Int
  deriving stock (Eq, Ord, Show)

-- | The hour if it is in range, else 'Nothing'.
mkHour :: Int -> Maybe Hour
mkHour h
  | h >= 0 && h < 24 = Just (Hour h)
  | otherwise = Nothing

hourInt :: Hour -> Int
hourInt (Hour h) = h

-- | A TCP port, @1..65535@ by construction ('Word16' caps the top, 'mkPort'
-- rejects @0@).
newtype Port = Port Word16
  deriving stock (Eq, Ord, Show)

-- | The port if it is in range (@1..65535@), else 'Nothing'.
mkPort :: Int -> Maybe Port
mkPort n
  | n >= 1 && n <= 65535 = Just (Port (fromIntegral n))
  | otherwise = Nothing

portNumber :: Port -> Int
portNumber (Port p) = fromIntegral p

data Config = Config
  { cfgDbPath      :: FilePath
  , cfgQueueDir    :: FilePath
  , cfgProofDir    :: FilePath
  , cfgMediaDir    :: FilePath
  -- ^ Where pet-report keeps its OWNED copies of a kept moment's still and clip, so a
  -- keepsake survives Frigate pruning. Keyed by Frigate event id, populated on Keep, and
  -- served owned-first by the event-media routes.
  , cfgListen      :: Text
  , cfgFrigateUrl  :: Text
  , cfgLlamaUrl    :: Text
  , cfgNtfyUrl     :: Text
  , cfgVisionModel :: Text
  , cfgPublicUrl   :: Maybe Text
  -- ^ Public URL of the web UI, wired to the ntfy tap-through (Click) header so a
  -- push opens the app. 'Nothing' (the default) leaves the notification link off.
  , cfgTimeZone    :: Text
  -- ^ The IANA zone name (e.g. @Europe/Lisbon@) used for day boundaries and the
  -- morning/evening split. The profile's @timeZone@ overrides this env default, and both
  -- fall back to UTC for an unknown name.
  , cfgCameras     :: [Camera]
  , cfgPetLabels   :: [Text]
  -- ^ Frigate object labels ingested as visual pet sightings (@dog@, @cat@).
  , cfgAudioLabels :: [Text]
  -- ^ Frigate audio-detection labels to ingest as sound events (barks, meows,
  -- the doorbell, and safety sounds like a smoke alarm or breaking glass).
  , cfgRetentionPollSecs :: NominalDiffTime
  -- ^ How often the retention poller re-reads Frigate's config to refresh the
  -- per-camera media retention that drives a moment's clip-expiry countdown.
  -- Retention changes rarely, so this is coarse (default 15 minutes).
  , cfgCaptureSecs :: NominalDiffTime
  -- ^ How often the serve process's capture loop queues a fresh frame per online camera.
  -- Default 10 minutes.
  , cfgBatchHours  :: [Hour]
  -- ^ The local hours at which the serve process runs the batch: analysis, report, then
  -- cleanup. Default 8 and 20.
  }
  deriving stock (Show)

-- | Load the config from the environment, parsing each field to its precise type and
-- failing fast. A 'Left' lists every problem at once (unparseable URL, unknown zone, a
-- listen address that is not host:port, an out-of-range batch hour), so the caller reports
-- them together rather than crashing on the first one later.
loadConfig :: IO (Either [Text] Config)
loadConfig = do
  -- Default to a writable per-user data dir (XDG, e.g. ~/.local/share/pet-report)
  -- so a fresh checkout runs with no env at all. The deployed service sets these
  -- to an explicit data dir (e.g. /var/lib/pet-report).
  dataDir <- getXdgDirectory XdgData "pet-report"
  db      <- envStr "PET_REPORT_DB" (dataDir </> "pet-report.db")
  queue   <- envStr "PET_REPORT_QUEUE" (dataDir </> "queue")
  proof   <- envStr "PET_REPORT_PROOF_DIR" (dataDir </> "proof")
  media   <- envStr "PET_REPORT_MEDIA_DIR" (dataDir </> "media")
  listen  <- envTxt "PET_REPORT_LISTEN" "127.0.0.1:8116"
  frigate <- envTxt "FRIGATE_URL" "http://localhost:8114"
  llama   <- envTxt "LLAMA_SWAP_URL" "http://localhost:8080"
  ntfy    <- envTxt "NTFY_URL" "http://localhost:8106/pet-report"
  -- There is no honest default here, since the name is whatever the owner's server calls
  -- the model. The placeholder is non-empty only because validation rejects empty and
  -- startup has to survive long enough to reach the setup page. A single-model llama.cpp
  -- ignores the field and serves whatever is loaded; llama-swap needs the alias.
  model   <- envTxt "VISION_MODEL" "vision-model"
  public  <- envMaybe "PET_REPORT_PUBLIC_URL"
  tz      <- envTxt "PET_REPORT_TZ" "UTC"
  cams    <- envTxt "PET_CAMERAS" "office"
  petlbl  <- envTxt "PET_LABELS" "dog,cat"
  audio   <- envTxt "PET_AUDIO_LABELS" defaultAudioLabels
  pollsec <- envInt "PET_REPORT_RETENTION_POLL_SECS" (900 :: Int)
  capsec  <- envInt "PET_REPORT_CAPTURE_SECS" (600 :: Int)
  -- Unset (or all-invalid) falls through to 'defaultBatchHours' rather than repeating
  -- the 8/20 default here.
  bhours  <- envTxt "PET_REPORT_BATCH_HOURS" ""
  let (badHours, batchHours) = parseHours bhours
      cfg =
        Config
          { cfgDbPath = db
          , cfgQueueDir = queue
          , cfgProofDir = proof
          , cfgMediaDir = media
          , cfgListen = listen
          , cfgFrigateUrl = frigate
          , cfgLlamaUrl = llama
          , cfgNtfyUrl = ntfy
          , cfgVisionModel = model
          , cfgPublicUrl = public
          , cfgTimeZone = tz
          , cfgCameras = map Camera (splitList cams)
          , cfgPetLabels = splitList petlbl
          , cfgAudioLabels = splitList audio
          , cfgRetentionPollSecs = fromIntegral pollsec
          , cfgCaptureSecs = fromIntegral capsec
          , cfgBatchHours = if null batchHours then defaultBatchHours else batchHours
          }
      problems =
        concat
          [ ["FRIGATE_URL is not a valid URL: " <> frigate | not (resolvesToHttp frigate)]
          , ["LLAMA_SWAP_URL is not a valid URL: " <> llama | not (resolvesToHttp llama)]
          , ["NTFY_URL is not a valid URL: " <> ntfy | not (resolvesToHttp ntfy)]
          , ["PET_REPORT_PUBLIC_URL is not a valid URL: " <> u | Just u <- [public], not (resolvesToHttp u)]
          , ["VISION_MODEL must not be empty" | T.null (T.strip model)]
          , ["PET_REPORT_TZ is not a known IANA zone: " <> tz | isNothing (tzByName (encodeUtf8 tz))]
          , ["PET_REPORT_LISTEN must be host:port: " <> listen | isNothing (parseListen listen)]
          , ["PET_REPORT_BATCH_HOURS has hours outside 0-23: " <> T.intercalate ", " badHours | not (null badHours)]
          ]
  pure (if null problems then Right cfg else Left problems)
  where
    envStr k d = fromMaybe d <$> lookupEnv k
    envTxt k d = maybe d T.pack <$> lookupEnv k
    -- An integer env with a default; a malformed value falls back to the default
    -- rather than failing, since this is a coarse operational knob.
    envInt k d = maybe d (fromMaybe d . readMaybe) <$> lookupEnv k
    -- An optional text env: unset, or set to blank, is 'Nothing'; otherwise the
    -- trimmed value. Used for 'PET_REPORT_PUBLIC_URL', which has no default.
    envMaybe k = (>>= nonBlank . T.pack) <$> lookupEnv k
    splitList = filter (not . T.null) . T.splitOn ","

-- | Parse a comma-separated hour list, returning the tokens that are not valid
-- @0..23@ hours (for a fail-fast message) and the hours that are.
parseHours :: Text -> ([Text], [Hour])
parseHours raw =
  foldr step ([], []) (filter (not . T.null) (map T.strip (T.splitOn "," raw)))
  where
    step t (bad, good) = case mkHour =<< readMaybe (T.unpack t) of
      Just h  -> (bad, h : good)
      Nothing -> (t : bad, good)

-- | The batch hours used when none are configured: morning and evening.
defaultBatchHours :: [Hour]
defaultBatchHours = mapMaybe mkHour [8, 20]

-- | Parse a @host:port@ listen address, splitting on the LAST ':' so a bracketed
-- IPv6 host (@[::1]:8116@) keeps its port. Brackets are stripped from the host
-- (warp's host preference wants the bare address), and the port is validated by
-- construction. Both 'loadConfig' and the warp runner parse through here, so startup
-- cannot accept an address the runtime then fails to bind.
parseListen :: Text -> Maybe (Text, Port)
parseListen l = case T.breakOnEnd ":" l of
  (hostColon, p)
    | Just host <- T.stripSuffix ":" hostColon
    , not (T.null host)
    , Just n <- readMaybe (T.unpack p)
    , Just port <- mkPort n ->
        Just (stripBrackets host, port)
  _ -> Nothing
  where
    stripBrackets h = fromMaybe h (T.stripSuffix "]" h >>= T.stripPrefix "[")

-- | Frigate audio labels ingested by default (owner-overridable via the env).
-- Covers pet vocalisations, arrivals, and safety sounds.
defaultAudioLabels :: Text
defaultAudioLabels =
  "bark,bow-wow,howl,growling,whimper_dog,meow,speech,yell,doorbell,ding-dong,\
  \knock,fire_alarm,smoke_detector,siren,car_alarm,glass,shatter,breaking,thump,bang"
