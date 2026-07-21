-- | The curated pet identification guide. The owner writes free-form pet descriptions, and a
-- text-model meta-prompt turns them into something concise that still works under infrared,
-- which the vision, narrative and Ask prompts all read to tell the pets apart.
--
-- The result is cached in the @state@ KV table under the @roster_brief@ keys, keyed by a
-- hash of the pets' identity inputs and regenerated when those change. Every consumer falls
-- back to the raw 'petBrief' when the cache is missing or stale, so a model outage never
-- blocks.
module PetReport.Analysis.IdentGuide
  ( rosterHash
  , identGuide
  , refreshIfStale
  ) where

import           Control.Monad (when)
import           Data.List     (sortOn)
import           Data.Maybe    (fromMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           LLM.Call                  (Call (..), Sampling (..), note, prose,
                                            refine, runMaybe, system, userText)
import           PetReport.Domain.Profile  (Pet (..), Profile (..), activePets)
import           PetReport.Domain.Types    (speciesText)
import qualified PetReport.Effect.Db       as Db
import qualified PetReport.Effect.Llm      as Llm
import           PetReport.Analysis.Vocabulary (householdVisionNote, petBrief)
import           PetReport.Util            (nonBlank)

briefKey, briefHashKey :: Text
briefKey = "roster_brief"
briefHashKey = "roster_brief_hash"

-- | A stable fingerprint of the pets' identity inputs: name, species, description, note.
-- Sorted, so roster ordering never changes the hash, with control-character separators to
-- keep it unambiguous. When this differs from the cached hash, the brief is stale.
rosterHash :: Profile -> Text
rosterHash prof =
  T.intercalate "\RS"
    [ T.intercalate "\US" [petName p, speciesText (petSpecies p), petDescription p, fromMaybe "" (petNotes p)]
    | p <- sortOn petName (activePets prof)
    ]

-- | The identification guide to inject into a prompt: the cached curated guide when it
-- matches the current roster, otherwise the raw 'petBrief' fallback.
identGuide :: Db.Handle -> Profile -> IO Text
identGuide db prof = do
  mh <- Db.getState db briefHashKey
  mb <- Db.getState db briefKey
  -- Only the pet-identity brief is cached. The household note is appended live, so
  -- toggling a cat flap or a neighbour's cat takes effect without a regeneration.
  let base = case (mh, mb) of
        (Just h, Just b) | h == rosterHash prof -> b
        _                                       -> petBrief prof
      hv = householdVisionNote (household prof)
  pure (if T.null hv then base else base <> "\n" <> hv)

-- | Regenerate the cached brief only when the roster's fingerprint has changed since it was
-- last built. The staleness protocol lives here, so callers just fire this after a profile
-- change.
refreshIfStale :: Llm.Handle -> Db.Handle -> Profile -> IO ()
refreshIfStale llm db prof = do
  cached <- Db.getState db briefHashKey
  when (cached /= Just (rosterHash prof)) (refreshGuide llm db prof)

-- | Regenerate the curated guide and cache it. A model failure or an empty roster leaves
-- the cache untouched, and the hash guard makes a stale entry fall back to 'petBrief'. Safe
-- to run in the background after a profile change.
refreshGuide :: Llm.Handle -> Db.Handle -> Profile -> IO ()
refreshGuide llm db prof = case activePets prof of
  [] -> pure ()
  _  -> do
    guide <- runMaybe llm (guideCall prof)
    case guide of
      Nothing -> pure ()
      Just t -> do
        Db.setState db briefKey t
        Db.setState db briefHashKey (rosterHash prof)

-- | The guide-generation task: the meta-prompt as the system message, the raw 'petBrief' as
-- the user message, and a generous budget, since it reasons over the whole roster. A blank
-- prose reply reduces to 'Nothing', which leaves the cache untouched.
guideCall :: Profile -> Call Text
guideCall prof =
  Call
    { callMessages = [system metaSystem, userText (petBrief prof)]
    , callSampling = Sampling {samplingTemperature = 0.2, samplingMaxTokens = 600, samplingThinking = False}
    , callBudget = Llm.briefBudget
    , callReply = refine (note "the identification guide came back blank" . nonBlank) prose
    }

metaSystem :: Text
metaSystem =
  T.intercalate
    "\n"
    [ "You are preparing an identification guide for a home pet-monitoring vision model. The model reads it on every camera frame to tell the household's pets apart, INCLUDING night-vision infrared (black and white), where colour cannot be trusted."
    , "From the owner's pet descriptions, write a short, factual guide. For EACH pet, lead with traits that survive infrared: size and build, tail length and shape, ear shape, head and face shape, coat length and texture, gait, and the shape and placement (not the colour) of any markings."
    , "End each pet's entry with a line beginning \"To identify: \" that names the two or three most reliable cues to recognise that pet at a glance - prefer a distinctive permanent feature first, then size, shape, ears, and gait; never colour alone."
    , "State plainly how to tell apart any pets that share a species. If a pet has a distinctive permanent feature (a missing limb, a bobtail, a notched ear), use it as a strong identification cue, and note it is normal for that pet and must never be reported as an injury or concern."
    , "Where a description gives only colour, say the model should fall back to species and lower its confidence."
    , "Output plain text: a few lines per pet, no preamble, no markdown headers."
    ]
