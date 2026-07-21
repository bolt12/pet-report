-- | The daily narrative: turn a window of observations into timestamped lines, wrap them in
-- the profile-steered instruction, and ask the text model for a short grounded summary. A
-- 'Call' with a 'prose' reply. The budget is caller-supplied, since the batch and the
-- on-demand recap want opposite timeouts.
module PetReport.Analysis.Narrative
  ( synthesize
  , observationLine
  ) where

import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Zones (TZ)

import           LLM.Call                     (Call (..), Sampling (..), prose,
                                               runMaybe, userText)
import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.Perception  (Appearance (..), Perception (..),
                                               Scene (..), Who (..), soundPhrase)
import           PetReport.Domain.Profile     (Household (..), Profile (..),
                                               ReportPrefs (..), ReportTopic (..))
import           PetReport.Domain.Stats       (Presence (..), homePresence)
import           PetReport.Domain.Types       (Species (..), activityText, cameraText)
import           PetReport.Domain.Window      (localClock)
import           PetReport.Effect.Llm         (CallBudget, Handle)
import           PetReport.Util               (boundedLines, nonBlank, paragraphs)

-- | Synthesise the day's narrative. The background batch and the interactive recap endpoint
-- both call this, and they want opposite timeout budgets, so the budget is an argument
-- rather than baked in here.
synthesize :: Handle -> CallBudget -> TZ -> Profile -> Text -> [Observation] -> IO Text
synthesize llm budget tz prof brief obss = do
  let body = boundedLines 200 (map (observationLine tz) obss)
      someoneHome = prSomeoneHome (homePresence obss)
  fromMaybe "No summary available yet." <$> runMaybe llm (narrativeCall budget prof brief someoneHome body)

narrativeCall :: CallBudget -> Profile -> Text -> Bool -> Text -> Call Text
narrativeCall budget prof brief someoneHome body =
  Call
    { callMessages = [userText (narrativeInstructions prof brief someoneHome body)]
    , callSampling = Sampling {samplingTemperature = 0.6, samplingMaxTokens = 800, samplingThinking = False}
    , callBudget = budget
    , callReply = prose
    }

-- | Instruction for the daily narrative, wrapping the observation lines in @body@.
-- @someoneHome@ softens the tone: with a person present in the window, human activity is
-- normal and must never read as an intruder.
narrativeInstructions :: Profile -> Text -> Bool -> Text -> Text
narrativeInstructions prof brief someoneHome body =
  paragraphs
    [ "You are writing a short, warm daily update for a pet owner about their pets and home."
    , brief
    , reportFocus (report prof)
    , maybe "" ("The owner also cares about: " <>) (freeform (report prof))
    , householdBrief (household prof)
    , if someoneHome
        then "Someone was home during this window, so treat human activity as normal and keep the tone reassuring; never raise an intruder alarm about a person."
        else ""
    , "Lead with anything concerning, a safety sound (a smoke alarm, breaking glass, a siren), a possible accident, or a possible injury, and give the emphasised topics above the most prominence."
    , "Base everything ONLY on the observations below; never invent. Respect any mis-identification notes. Be concise (4 to 8 lines)."
    , ""
    , body
    ]

reportFocus :: ReportPrefs -> Text
reportFocus rp =
  let ts = Set.toList (topics rp)
   in if null ts
        then ""
        else "Emphasise these topics: " <> T.intercalate ", " (map topicText ts) <> "."

topicText :: ReportTopic -> Text
topicText t = case t of
  Meals            -> "meals"
  Water            -> "water intake"
  Litter           -> "litter and toileting"
  Sleep            -> "sleep and rest"
  Play             -> "play and activity"
  Grooming         -> "grooming"
  Visitors         -> "visitors and people"
  Sounds           -> "notable sounds (barks, meows, the doorbell)"
  Outdoors         -> "time outdoors (the garden, cat-flap trips)"
  Health           -> "health concerns"
  UnusualBehaviour -> "unusual behaviour"

-- | The narrative-only household context, feed times and free-form notes, folded into one
-- steering line. Empty when there is none. Cat flap and neighbour's cat live in
-- 'PetReport.Analysis.Vocabulary.householdVisionNote' instead, since those steer
-- identification rather than tone.
householdBrief :: Household -> Text
householdBrief h =
  let feed = case feedTimes h >>= nonBlank of
        Just t  -> ["usual feed times are " <> t]
        Nothing -> []
      extra = case notes h >>= nonBlank of
        Just t  -> [t]
        Nothing -> []
      bits = feed ++ extra
   in if null bits
        then ""
        else "Household context: " <> T.intercalate "; " bits <> "."

-- | One observation as the models see it: @HH:MM [camera] description@ in local time. The
-- daily narrative body, the per-pet weekly body and the Ask agent's tools all render through
-- here, so every prompt shows an observation the same way.
observationLine :: TZ -> Observation -> Text
observationLine tz obs =
  let hhmm = localClock tz (at obs)
   in hhmm
        <> " ["
        <> cameraText (camera obs)
        <> "] "
        <> perceptionText (perception obs)

perceptionText :: Perception -> Text
perceptionText p = case p of
  Seen sc ->
    fromMaybe
      (T.intercalate "; " (map appearanceText (appearances sc)))
      (description sc)
  Heard sk -> "microphone picked up " <> soundPhrase sk

appearanceText :: Appearance -> Text
appearanceText ap =
  whoText (who ap)
    <> " "
    <> activityText (activity ap)
    <> maybe "" (" " <>) (whereAt ap)

whoText :: Who -> Text
whoText w = case w of
  APerson              -> "a person"
  AnAnimal (Species s) -> s
