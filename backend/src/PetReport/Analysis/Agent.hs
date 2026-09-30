-- | The "Ask" agent: pet-report's read-only tools plus the prompt that steers them, run over
-- the harness loop in "LLM.Agent". The loop mechanics live in the harness; this module
-- supplies the domain half, being six tools that fetch facts from the database and Frigate,
-- the system prompt, and the run budgets.
--
-- Alongside its JSON result, a moment-returning tool hands back the ids of the observations
-- it surfaced, accumulating in the loop's @w = [ObsId]@. The caller can then enrich exactly
-- the moments the agent looked at into cards, rather than re-guessing them from the question.
module PetReport.Analysis.Agent
  ( ask
  ) where

import           Data.Aeson                    (Value, object, (.=))
import           Data.Aeson.Types              (parseEither)
import           Data.Either                   (fromRight)
import           Data.List                     (nub, sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Ord                      (Down (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (NominalDiffTime, UTCTime,
                                                addUTCTime)
import           Data.Time.Zones               (TZ)

import           Autodocodec                   (HasCodec (..), parseJSONViaCodec)
import qualified Autodocodec                   as Ac

import           LLM.Agent                     (AgentTurn (..), RunBudget (..),
                                                Tool (..), runAgent)
import           LLM.Call                      (Sampling (..))
import           LLM.Schema                    (codecSchema)
import qualified PetReport.Analysis.IdentGuide as IdentGuide
import qualified PetReport.Analysis.Narrative  as Narrative
import           PetReport.App                 (App (..))
import           PetReport.Domain.Observation  (obsId)
import           PetReport.Domain.Profile      (Pet (..), Profile (..),
                                                enabledCameras, petById, roomOf)
import qualified PetReport.Domain.Stats        as Stats
import           PetReport.Domain.Types        (Camera (..), ObsId (..),
                                                Species (..))
import           PetReport.Domain.Window       (Window (..), localClock,
                                                localDayOf, parseWindow)
import qualified PetReport.Effect.Clock        as Clock
import qualified PetReport.Effect.Db           as Db
import qualified PetReport.Effect.Frigate      as Frigate
import qualified PetReport.Effect.Llm          as Llm

maxIters :: Int
maxIters = 5

-- | The wall-clock budget for the whole ask loop. It runs in-request on a warp handler
-- thread, so it has to return before the ~120s client timeout even if every iteration
-- stalls. Kept under that ceiling, the endpoint answers with the best it has instead of a
-- slow model pinning the thread past the point the client gave up.
askBudgetSecs :: NominalDiffTime
askBudgetSecs = 100

-- | Backstop cap on one tool result's serialised size: roughly 5000 tokens at the usual
-- four-chars-per-token heuristic, and a generous multiple of any bounded tool's real output.
-- The loop refuses anything larger with a "narrow your query" signal rather than truncating.
-- Results accumulate across 'maxIters' turns, so this stays well under the model's context.
maxToolResultChars :: Int
maxToolResultChars = 20000

-- | Answer an owner question, returning the answer plus the ids of the observations the
-- agent surfaced through its moment tools, deduped and most-recent-first. The caller enriches
-- those into cards, so the refs it shows are the moments the agent actually looked at rather
-- than a guess from the question.
--
-- @now@, @tz@ and @prof@ come from the caller, which has already loaded them, so the agent
-- does not re-read the profile just to build its system prompt. The tools still read their
-- own clock and database from the 'App'. The identification guide is derived here, being
-- agent-specific.
ask :: App -> Profile -> UTCTime -> TZ -> Text -> IO (Text, [ObsId])
ask app prof now tz question = do
  guide <- IdentGuide.identGuide (appDb app) prof
  let localDate = T.pack (show (localDayOf tz now))
      localTime = localClock tz now
      msgs0 =
        [ Llm.ChatMessage Llm.System (Llm.ContentText (askSystemPrompt prof guide localDate localTime)) [] Nothing
        , Llm.ChatMessage Llm.User (Llm.ContentText question) [] Nothing
        ]
      budget =
        RunBudget
          { rbMaxIters = maxIters
          , rbDeadline = addUTCTime askBudgetSecs now
          , rbMaxToolResultChars = maxToolResultChars
          }
      -- ask runs in-request on a warp handler thread, so each turn takes the interactive
      -- budget: one attempt under a ~100s cap, nested inside the loop's askBudgetSecs
      -- deadline and the client timeout beyond that. Low temperature for a fact-grounded
      -- agent that must not guess, thinking on, with headroom for the trace.
      turn =
        AgentTurn
          { atSampling = Sampling {samplingTemperature = 0.2, samplingMaxTokens = 1800, samplingThinking = True}
          , atBudget = Llm.interactiveBudget
          }
  (answer, surfaced) <- runAgent (appLlm app) (Clock.now (appClock app)) budget turn noAnswer (agentTools app) msgs0
  pure (answer, recentFirst surfaced)

-- | A tool's implementation: its JSON result for the model, plus the ids of any observations
-- it surfaced. Aggregate tools surface none; the two moment tools surface the observations
-- behind the lines they render.
type ToolFn = Value -> IO (Value, [ObsId])

-- | The six read-only tools the agent may call, each closing over the 'App' for its
-- database, clock and Frigate handles. Every tool's argument schema is codec-derived, so the
-- shape the model is told to send is the shape the decoder expects.
agentTools :: App -> [Tool [ObsId]]
agentTools app =
  [ Tool
      "activity_stats"
      "Per-pet activity over a window: sightings and ate/drank/slept/played/concern counts."
      (codecSchema @RangeArgs)
      (statsHandler app)
  , Tool
      "count_observations"
      "Count observations in a time window."
      (codecSchema @RangeArgs)
      (countHandler app)
  , Tool
      "daily_timeline"
      "Timestamped observation lines for a single day (default today)."
      (codecSchema @DayArg)
      (timelineHandler app)
  , Tool
      "search_observations"
      "Search a day's observations for a keyword (matches the description, subjects, room, or sound)."
      (codecSchema @SearchArgs)
      (searchHandler app)
  , Tool
      "camera_status"
      "Which configured cameras are currently online or offline."
      (codecSchema @NoArgs)
      (cameraStatusHandler app)
  , Tool
      "presence"
      "Whether a person was home in a time window (coarse; never identifies who): someone_home, person_sightings, last_person_time."
      (codecSchema @RangeArgs)
      (presenceHandler app)
  ]

-- | Order surfaced ids most-recent-first and dedup. Ids are monotonic in insert order, so
-- the larger id is the more recent moment, and the caller can take the top few by recency
-- without re-reading timestamps.
recentFirst :: [ObsId] -> [ObsId]
recentFirst = sortOn (\(ObsId i) -> Down i) . nub

-- | The fallback when the model produced no usable answer, or the deadline stopped the loop
-- before it did. The SPA renders this as-is.
noAnswer :: Text
noAnswer = "I could not find an answer in the footage."

-- | The Ask agent's system prompt: how to answer the owner's questions from the read-only
-- tools. @brief@ is the roster; @localDate@ and @localTime@ pin "today" so relative dates
-- resolve. Lives beside the tools it steers.
askSystemPrompt :: Profile -> Text -> Text -> Text -> Text
askSystemPrompt prof brief localDate localTime =
  T.intercalate
    "\n"
    [ "You answer the owner's questions about their pets, any visitors, and the home."
    , brief
    , "Use the read-only tools to fetch facts BEFORE answering; never guess counts, dates, or presence."
    , "Answer warmly and concisely: lead with the direct answer in a sentence or two, then any supporting detail. When a specific moment matters, give its clock time (e.g. 2:14 pm) so the owner can jump straight to it."
    , "Write plain conversational sentences only: no markdown, no asterisks or bullet lists, no headings. Your reply is shown as plain text, so formatting characters would appear literally."
    , "The camera AI sometimes mistakes one animal for another, so a species sighting is not proof of a specific pet."
    , "Eating, drinking and litter use are rarely visible to the cameras, so a zero count for them means \"not seen\", not \"did not happen\": say \"I didn't see ...\", never \"X did not ...\"."
    , "If the tools do not contain the answer, say plainly you cannot tell from the footage. Cameras are often off, so little data can simply mean the cameras were off."
    , "Today's local date is "
        <> localDate
        <> " (timezone "
        <> fromMaybe "local" (timeZone prof)
        <> "). Resolve relative dates (today, yesterday, this morning, a weekday) against it and pass concrete YYYY-MM-DD days to the tools. If the question names no day, assume today."
    , "If today has nothing relevant but a recent day does, say so and offer that day (for example: I don't see anything today; the last sightings were on 2026-07-12, want those?)."
    , "The current local time is " <> localTime <> "."
    ]

-- --------------------------------------------------------------------------- --
-- Tool arguments
-- --------------------------------------------------------------------------- --

-- | The window tools take an optional since/until pair, as one codec-backed record whose
-- schema and parser are the same declaration.
data RangeArgs = RangeArgs
  { raSince :: Maybe Text
  , raUntil :: Maybe Text
  }

instance HasCodec RangeArgs where
  codec =
    Ac.object "RangeArgs" $
      RangeArgs
        <$> Ac.optionalFieldOrNull "since" "window start: 'today', 'Nd', or YYYY-MM-DD" Ac..= raSince
        <*> Ac.optionalFieldOrNull "until" "window end: 'today'/'now' or YYYY-MM-DD" Ac..= raUntil

-- | @daily_timeline@ takes a single optional day.
newtype DayArg = DayArg
  { daDay :: Maybe Text
  }

instance HasCodec DayArg where
  codec =
    Ac.object "DayArg" $
      DayArg
        <$> Ac.optionalFieldOrNull "day" "'today' or YYYY-MM-DD" Ac..= daDay

-- | @search_observations@ takes an optional keyword and an optional day.
data SearchArgs = SearchArgs
  { saText :: Maybe Text
  , saDay  :: Maybe Text
  }

instance HasCodec SearchArgs where
  codec =
    Ac.object "SearchArgs" $
      SearchArgs
        <$> Ac.optionalFieldOrNull "text" "keyword to search for" Ac..= saText
        <*> Ac.optionalFieldOrNull "day" "'today' or YYYY-MM-DD (default today)" Ac..= saDay

-- | The empty-argument shape, for @camera_status@. Codec-derived like the rest, so every
-- tool's parameters come off one mechanism.
data NoArgs = NoArgs

instance HasCodec NoArgs where
  codec = Ac.object "NoArgs" (pure NoArgs)

-- | Decode tool arguments through a record's codec, so the parser is the schema. Malformed
-- or absent arguments fall back to the record's all-absent shape rather than failing the
-- tool, and the window parser then defaults to today.
decodeArgs :: (HasCodec a) => a -> Value -> a
decodeArgs fallback v = fromRight fallback (parseEither parseJSONViaCodec v)

-- --------------------------------------------------------------------------- --
-- Tools
-- --------------------------------------------------------------------------- --

statsHandler :: App -> ToolFn
statsHandler app args = do
  (lo, hi) <- rangeWindow app args
  let db = appDb app
  obss <- Db.observationsBetween db lo hi
  ov <- Db.overridesBetween db lo hi
  prof <- Db.getProfile db
  let pres = Stats.presence ov (pets prof) obss
  pure
    ( object
        [ "observations" .= length obss
        , "pets"
            .= [ object
                   [ "subject" .= keyLabel prof k
                   , "sightings" .= Stats.psSightings v
                   , "ate" .= Stats.psAte v
                   , "drank" .= Stats.psDrank v
                   , "slept" .= Stats.psSlept v
                   , "played" .= Stats.psPlayed v
                   , "concerns" .= Stats.psConcerns v
                   ]
               | (k, v) <- Map.toList (Stats.resolvedStatsMap pres)
               ]
        ]
    , []
    )

countHandler :: App -> ToolFn
countHandler app args = do
  (lo, hi) <- rangeWindow app args
  obss <- Db.observationsBetween (appDb app) lo hi
  pure (object ["count" .= length obss], [])

timelineHandler :: App -> ToolFn
timelineHandler app args = do
  let clock = appClock app
      db = appDb app
  now <- Clock.now clock
  tz <- Clock.timeZone clock
  let day = daDay (decodeArgs (DayArg Nothing) args)
      w = parseWindow tz now day day
  obss <- Db.observationsBetween db (winFrom w) (winTo w)
  -- Return the ids behind the lines. These are exactly the moments the model reads, and the
  -- caller surfaces them as the answer's refs.
  pure (object ["lines" .= map (Narrative.observationLine tz) obss], map obsId obss)

searchHandler :: App -> ToolFn
searchHandler app args = do
  let clock = appClock app
      db = appDb app
  now <- Clock.now clock
  tz <- Clock.timeZone clock
  let SearchArgs txt day = decodeArgs (SearchArgs Nothing Nothing) args
      q = maybe "" T.toLower txt
      w = parseWindow tz now day day
  obss <- Db.observationsBetween db (winFrom w) (winTo w)
  -- Keep the matched observation alongside its line, so the surfaced ids are exactly the
  -- moments rendered to the model, under the same filter and the same 'take' bound.
  let matched = [(o, l) | o <- obss, let l = Narrative.observationLine tz o, q `T.isInfixOf` T.toLower l]
      shown = take 40 matched
  pure
    ( object ["matches" .= length matched, "lines" .= map snd shown]
    , map (obsId . fst) shown
    )

presenceHandler :: App -> ToolFn
presenceHandler app args = do
  (lo, hi) <- rangeWindow app args
  obss <- Db.observationsBetween (appDb app) lo hi
  let p = Stats.homePresence obss
  pure
    ( object
        [ "someone_home" .= Stats.prSomeoneHome p
        , "person_sightings" .= Stats.prPersonSightings p
        , "last_person_time" .= fmap show (Stats.prLastPersonAt p)
        ]
    , []
    )

cameraStatusHandler :: App -> ToolFn
cameraStatusHandler app _ = do
  prof <- Db.getProfile (appDb app)
  online <- Frigate.onlineCameras (appFrigate app) (enabledCameras prof)
  let status = [(roomOf (cameras prof) (Camera c), c `elem` online) | c <- enabledCameras prof]
  pure
    ( object
        [ "online" .= [r | (r, True) <- status]
        , "offline" .= [r | (r, False) <- status]
        ]
    , []
    )

-- | Resolve a 'RangeArgs' value into a concrete window against the tool clock.
rangeWindow :: App -> Value -> IO (UTCTime, UTCTime)
rangeWindow app args = do
  let clock = appClock app
  now <- Clock.now clock
  tz <- Clock.timeZone clock
  let RangeArgs since untl = decodeArgs (RangeArgs Nothing Nothing) args
      w = parseWindow tz now since untl
  pure (winFrom w, winTo w)

keyLabel :: Profile -> Stats.SubjectKey -> Text
keyLabel prof k = case k of
  Stats.KPet pid             -> maybe "unknown pet" petName (petById (pets prof) pid)
  Stats.KSpecies (Species s) -> s
  Stats.KVisitor (Species s) -> "visiting " <> s
  Stats.KPerson              -> "person"
