-- | Structured, leveled logging via @contra-tracer@. Each subsystem emits its own typed
-- event through a 'Tracer' it receives, and the only rendering interpretation lives at the
-- root ('renderingTracerAt'). A subsystem gets its @Tracer IO XEvent@ by 'contramap'-ping
-- the root tracer with its constructor, as 'pipelineTracer' does.
--
-- Severity belongs to the event and is assigned by the render function, never chosen at the
-- call site. The root tracer drops anything below its minimum level (see 'logLevelFromEnv'
-- and @PET_REPORT_LOG_LEVEL@), so routine 'Debug' detail stays hidden until one env var
-- turns it on.
module PetReport.Trace
  ( Trace (..)
  , Severity (..)
  , StartupEvent (..)
  , DbEvent (..)
  , PipelineEvent (..)
  , SkipReason (..)
  , WebEvent (..)
  , NtfyEvent (..)
  , Tracer
  , traceWith
  , renderingTracer
  , renderingTracerAt
  , logLevelFromEnv
  , resolveLogLevel
  , severityFromText
  , sevText
    -- * Subsystem sub-tracers (from the root tracer)
  , startupTracer
  , pipelineTracer
  , webTracer
  , dbTracer
  , ntfyTracer
  , llmTracer
  ) where

import           Control.Monad      (when)
import           Control.Tracer     (Tracer (..), contramap, emit, traceWith)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Time          (defaultTimeLocale, formatTime,
                                     getCurrentTime)
import           System.Environment (lookupEnv)
import           System.IO          (stderr)

import           LLM.Trace          (ChatOutcome (..), LlmEvent (..))
import           PetReport.Util     (nonBlank, tshow)

-- | Log severity, ordered low to high so a minimum-level threshold is a simple comparison
-- (@sev >= minLevel@). 'Debug' is the routine detail hidden at the default level.
data Severity = Debug | Info | Warning | Error
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The label a severity is printed with, and through 'severityFromText' the name that
-- selects it in @PET_REPORT_LOG_LEVEL@. One table for both directions, so what you set is
-- what you read back in the logs.
sevText :: Severity -> Text
sevText s = case s of
  Debug   -> "DEBUG"
  Info    -> "INFO"
  Warning -> "WARN"
  Error   -> "ERROR"

data WithSeverity a = WithSeverity Severity a
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------------- --
-- Events
-- --------------------------------------------------------------------------- --

-- | Startup and one-off CLI events.
data StartupEvent
  = ConfigInvalid Text
  -- ^ A startup configuration problem (fatal); one per problem found.
  | Reprojected Int
  -- ^ The @reproject@ command rebuilt the facts projection from N observations.
  | UnknownLogLevel Text
  -- ^ @PET_REPORT_LOG_LEVEL@ was set to a name that is not a level; 'Info' was used. Carries
  -- the offending value so the owner can see what was ignored.
  deriving stock (Eq, Show)

-- | Database events: schema migrations at open time, and rows the query layer
-- could not decode (skipped rather than failing the whole read).
data DbEvent
  = SchemaTooNew Text
  -- ^ The database schema is newer than this build understands (fatal); the text is
  -- the actionable message.
  | SchemaUnversioned Text
  -- ^ The database holds tables but carries no schema version, so it predates the
  -- versioned runner and cannot be migrated (fatal); the text is the actionable message.
  | ApplyingMigration Int
  -- ^ Applying the schema migration to version N (emitted before it runs).
  | SchemaMigrated Int
  -- ^ The schema was brought up to version N.
  | SchemaUpToDate Int
  -- ^ The schema was already at version N; nothing to do.
  | CorruptProfileJson Text
  -- ^ The stored profile JSON did not decode; the default was used. Carries the error.
  | UnreadableRow Text Text
  -- ^ A row could not be decoded and was skipped. Carries the reading context (e.g.
  -- @"browse"@, @"purge"@) and the decode error.
  deriving stock (Eq, Show)

-- | Why an ingested Frigate event was skipped and left unstored, with the ingest watermark
-- advancing past it. Each constructor is a distinct place a sighting silently disappears, so
-- ingest can say which one a given event took instead of the drop being invisible.
data SkipReason
  = SkippedNoMedia
  -- ^ A completed event carrying neither a snapshot nor a clip: nothing to analyse.
  | SkippedCooldown
  -- ^ Within the per-@(camera, label)@ cooldown of an already-stored sighting; deduped.
  | SkippedFalsePositive
  -- ^ Frigate flagged the event a false positive.
  | SkippedAbandoned
  -- ^ Its media stayed unreadable past the retry window, so ingest gave up rather than
  -- freezing behind it forever.
  deriving stock (Eq, Show)

-- | Pipeline events: the capture/analyse/report/cleanup automation and the
-- background worker and schedulers that drive it.
data PipelineEvent
  = FramesQueued Int Int
  -- ^ Queued N frames from M cameras this capture pass. M counts the cameras that were both
  -- online and quiet, since one Frigate has just reported on is covered by that event and is
  -- deliberately not sampled.
  | BatchAlreadyRunning
  -- ^ A batch was requested while one was already running; skipped.
  | BriefRefreshFailed Text
  -- ^ The identification brief could not be refreshed. Carries the error.
  | ModelUnavailable Text
  -- ^ The model was unreachable, so queued frames were deferred. Carries the error.
  | EventIngestFailed Text
  -- ^ Ingesting a Frigate event failed; the watermark held. Carries the error.
  | EventsFetched Int Int Int
  -- ^ Frigate returned N events for the ingest window @[after, before)@ (epoch seconds), the
  -- pool the per-event 'EventSkipped' traces then account for. Together they tell an empty
  -- window (nothing returned) apart from a full one whose events were all skipped.
  | EventSkipped SkipReason Text Text
  -- ^ An event (id, label) was NOT stored and the watermark advanced past it. The reason
  -- names which silent-drop path it took, so a vanished sighting is explained in the log
  -- rather than just missing from the report.
  | IngestStalled Int
  -- ^ A pass fetched N events and advanced the watermark past NONE of them, so the next pass
  -- will re-fetch exactly the same window. One pass is ordinary (a budget ran out, media was
  -- not ready yet); every pass means ingest is wedged and no sighting will ever be recorded.
  -- 'EventsFetched' is Debug, so without this a stall is invisible at the default log level.
  | IngestWatermarkStale Int
  -- ^ The stored watermark was N days behind and has been floored at Frigate's own media
  -- retention. Beyond that horizon the clips are gone, so those events could only be fetched,
  -- found media-less and skipped, one budget unit at a time.
  | GcCollected Int Text
  -- ^ Cleanup collected N un-kept moments from the given day.
  | NoObservationsForReport
  -- ^ A day had no observations, so no report was written.
  | ReportStored Bool
  -- ^ A daily report was stored; the flag is whether a push notification was sent.
  | PetSummariesFailed Text
  -- ^ Refreshing the per-pet summaries failed. Carries the error.
  | SchedulerStepFailed Text Text
  -- ^ A scheduler step (the first field, e.g. @"capture"@) was skipped after a
  -- failure (the second field).
  | RetentionUpdated Text
  -- ^ Frigate's media retention changed; the text summarises the new per-camera days.
  | JobFailed Text Text
  -- ^ A background job (the first field, e.g. @"batch"@) failed (the second field);
  -- the worker carries on.
  deriving stock (Eq, Show)

-- | Web events: the server lifecycle and per-request outcomes.
data WebEvent
  = Listening Text
  -- ^ The server is listening on the given address.
  | UnhandledException Text
  -- ^ An exception escaped a handler and became a 500. Carries the cause.
  | RequestServed Text Text Int
  -- ^ A request (method, path) completed with the given status. 'Debug' for a 2xx or 3xx
  -- and 'Warning' for a 4xx or 5xx, so normal traffic stays quiet while failures show. Set
  -- @PET_REPORT_LOG_LEVEL=debug@ to see every request.
  deriving stock (Eq, Show)

-- | Notification (ntfy) events.
data NtfyEvent
  = PushFailed Text
  -- ^ A push notification could not be sent. Carries the error.
  | InvalidNtfyUrl Text
  -- ^ The configured ntfy URL did not resolve; the push was dropped.
  deriving stock (Eq, Show)

-- | The closed set of trace event sources: one constructor per subsystem, each
-- carrying that subsystem's typed event.
data Trace
  = TraceStartup StartupEvent
  | TracePipeline PipelineEvent
  | TraceWeb WebEvent
  | TraceDb DbEvent
  | TraceNtfy NtfyEvent
  | TraceLlm LlmEvent
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------------- --
-- Rendering
-- --------------------------------------------------------------------------- --

renderTrace :: Trace -> WithSeverity Text
renderTrace tr =
  WithSeverity sev ("[" <> sevText sev <> "] [" <> comp <> "] " <> msg)
  where
    (comp, WithSeverity sev msg) = case tr of
      TraceStartup e  -> ("startup", renderStartup e)
      TracePipeline e -> ("pipeline", renderPipeline e)
      TraceWeb e      -> ("web", renderWeb e)
      TraceDb e       -> ("db", renderDb e)
      TraceNtfy e     -> ("ntfy", renderNtfy e)
      TraceLlm e      -> ("llm", renderLlm e)

renderStartup :: StartupEvent -> WithSeverity Text
renderStartup e = case e of
  ConfigInvalid m -> WithSeverity Error m
  Reprojected n   -> WithSeverity Info ("reprojected " <> tshow n <> " observations")
  UnknownLogLevel raw ->
    WithSeverity
      Warning
      ( "PET_REPORT_LOG_LEVEL is not a level name: " <> raw <> "; using info. Accepted: "
          <> T.intercalate ", " (map (T.toLower . sevText) [minBound .. maxBound])
      )

renderDb :: DbEvent -> WithSeverity Text
renderDb e = case e of
  SchemaTooNew m       -> WithSeverity Error m
  SchemaUnversioned m  -> WithSeverity Error m
  ApplyingMigration v  -> WithSeverity Debug ("applying schema migration " <> tshow v)
  SchemaMigrated v     -> WithSeverity Info ("schema migrated to version " <> tshow v)
  SchemaUpToDate v     -> WithSeverity Debug ("schema up to date at version " <> tshow v)
  CorruptProfileJson m -> WithSeverity Warning ("corrupt profile JSON, using default: " <> m)
  UnreadableRow ctx m  -> WithSeverity Warning (ctx <> " skipped an unreadable row: " <> m)

renderPipeline :: PipelineEvent -> WithSeverity Text
renderPipeline e = case e of
  FramesQueued saved cams ->
    WithSeverity Debug ("queued " <> tshow saved <> " frame(s) from " <> tshow cams <> " quiet camera(s)")
  BatchAlreadyRunning     -> WithSeverity Debug "another batch is already running; skipping"
  BriefRefreshFailed m    -> WithSeverity Warning ("brief refresh skipped: " <> m)
  ModelUnavailable m      -> WithSeverity Warning ("model unavailable; deferring queued frames: " <> m)
  EventIngestFailed m     -> WithSeverity Warning ("event ingest failed: " <> m)
  EventsFetched n aft bef ->
    WithSeverity Debug ("frigate returned " <> tshow n <> " event(s) for [" <> tshow aft <> ", " <> tshow bef <> ")")
  EventSkipped reason eid lbl ->
    WithSeverity (skipSeverity reason) ("event " <> eid <> " (" <> lbl <> ") not stored: " <> skipText reason)
  IngestStalled n ->
    WithSeverity Warning ("ingest made no progress: " <> tshow n <> " event(s) fetched, watermark unmoved")
  IngestWatermarkStale d ->
    WithSeverity Warning ("ingest watermark was " <> tshow d <> " day(s) stale; clamped to frigate's media retention")
  GcCollected n day       -> WithSeverity Info ("gc: collected " <> tshow n <> " un-kept moment(s) from " <> day)
  NoObservationsForReport -> WithSeverity Debug "no observations for that day; skipping report"
  ReportStored pushed     -> WithSeverity Info (if pushed then "report stored and pushed" else "report stored")
  PetSummariesFailed m    -> WithSeverity Warning ("pet summaries skipped: " <> m)
  SchedulerStepFailed w m -> WithSeverity Warning (w <> " skipped: " <> m)
  RetentionUpdated s      -> WithSeverity Info ("frigate media retention updated: " <> s)
  JobFailed lbl m         -> WithSeverity Warning (lbl <> " failed: " <> m)

-- | The severity a skip traces at. A cooldown dedup or a Frigate-flagged false positive is
-- routine 'Debug'; a completed event dropped for want of any media is 'Info' (a real sighting
-- that never lands); an event abandoned unread past the retry window is a 'Warning'.
skipSeverity :: SkipReason -> Severity
skipSeverity r = case r of
  SkippedCooldown      -> Debug
  SkippedFalsePositive -> Debug
  SkippedNoMedia       -> Info
  SkippedAbandoned     -> Warning

skipText :: SkipReason -> Text
skipText r = case r of
  SkippedNoMedia       -> "completed event has no snapshot or clip to analyse"
  SkippedCooldown      -> "within cooldown of a recent sighting on the same camera and label"
  SkippedFalsePositive -> "frigate flagged it a false positive"
  SkippedAbandoned     -> "media stayed unreadable past the retry window"

renderWeb :: WebEvent -> WithSeverity Text
renderWeb e = case e of
  Listening addr          -> WithSeverity Info ("listening on " <> addr)
  UnhandledException m    -> WithSeverity Error ("unhandled: " <> m)
  RequestServed meth p st ->
    WithSeverity (if st >= 400 then Warning else Debug) (meth <> " " <> p <> " -> " <> tshow st)

renderNtfy :: NtfyEvent -> WithSeverity Text
renderNtfy e = case e of
  PushFailed m     -> WithSeverity Warning ("push failed: " <> m)
  InvalidNtfyUrl u -> WithSeverity Warning ("invalid ntfy URL: " <> u)

-- | A model round-trip: the request shape, what came back, and the latency. A completed
-- call is routine detail and traces at 'Debug'. One that did not complete is a 'Warning',
-- since its latency and request shape are what an operator needs and the caller's own error
-- path reports neither.
renderLlm :: LlmEvent -> WithSeverity Text
renderLlm (LlmChat structured tools maxTok ms outcome) =
  WithSeverity
    severity
    (kind <> toolsPart <> ", max " <> tshow maxTok <> " tok -> " <> outcomeText <> " in " <> tshow ms <> "ms")
  where
    kind = if structured then "structured call" else "chat call"
    toolsPart = if tools > 0 then ", " <> tshow tools <> " tool(s)" else ""
    -- Exhaustive rather than a wildcard, so adding an outcome fails the build here instead
    -- of silently defaulting to Debug.
    severity = case outcome of
      ToolCalls _ -> Debug
      Content     -> Debug
      NoContent   -> Debug
      Failed _    -> Warning
    outcomeText = case outcome of
      ToolCalls n -> "tool_calls(" <> tshow n <> ")"
      Content     -> "content"
      NoContent   -> "no content"
      Failed m    -> "failed: " <> m

-- | The trace-stream interpretation at a given minimum level: a timestamped, leveled line
-- to stderr for every event at or above @minLevel@, and nothing below it.
renderingTracerAt :: Severity -> Tracer IO Trace
renderingTracerAt minLevel = Tracer $ emit $ \e -> do
  let WithSeverity sev line = renderTrace e
  when (sev >= minLevel) $ do
    now <- getCurrentTime
    let ts = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now)
    TIO.hPutStrLn stderr (ts <> " " <> line)

-- | The default root tracer, at 'Info' (routine 'Debug' detail hidden). The serve path uses
-- 'renderingTracerAt' with the level from 'logLevelFromEnv'; tests use this default.
renderingTracer :: Tracer IO Trace
renderingTracer = renderingTracerAt Info

-- | Parse a log-level name, case-insensitively and ignoring surrounding space. Derived from
-- 'sevText' over every constructor, so a new level needs no second table.
severityFromText :: Text -> Maybe Severity
severityFromText t =
  lookup (T.toLower (T.strip t)) [(T.toLower (sevText s), s) | s <- [minBound .. maxBound]]

-- | Resolve a raw @PET_REPORT_LOG_LEVEL@ value to the level to use, plus the value itself
-- when it was something unrecognised. Unset or blank is 'Info' with nothing to report. A
-- name that does not parse is also 'Info', but is handed back so the caller can say so
-- rather than leaving the owner wondering why @debgu@ changed nothing. Pure, so the fallback
-- is testable without touching the process environment.
resolveLogLevel :: Maybe Text -> (Severity, Maybe Text)
resolveLogLevel mraw = case nonBlank =<< mraw of
  Nothing -> (Info, Nothing)
  Just raw -> case severityFromText raw of
    Just sev -> (sev, Nothing)
    Nothing  -> (Info, Just raw)

-- | The minimum log level from @PET_REPORT_LOG_LEVEL@, and the raw value when it was not a
-- level name. Read here rather than in 'PetReport.Config' because every problem
-- @loadConfig@ reports is fatal, and an unreadable log level is not: it falls back and
-- carries on.
logLevelFromEnv :: IO (Severity, Maybe Text)
logLevelFromEnv = resolveLogLevel . fmap T.pack <$> lookupEnv "PET_REPORT_LOG_LEVEL"

-- | Narrow the root tracer to a subsystem, which then traces its own event type and stays
-- ignorant of the 'Trace' sum.
startupTracer :: Tracer IO Trace -> Tracer IO StartupEvent
startupTracer = contramap TraceStartup

pipelineTracer :: Tracer IO Trace -> Tracer IO PipelineEvent
pipelineTracer = contramap TracePipeline

webTracer :: Tracer IO Trace -> Tracer IO WebEvent
webTracer = contramap TraceWeb

dbTracer :: Tracer IO Trace -> Tracer IO DbEvent
dbTracer = contramap TraceDb

ntfyTracer :: Tracer IO Trace -> Tracer IO NtfyEvent
ntfyTracer = contramap TraceNtfy

llmTracer :: Tracer IO Trace -> Tracer IO LlmEvent
llmTracer = contramap TraceLlm
