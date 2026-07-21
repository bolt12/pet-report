-- | Shared prompt vocabulary: the small, profile-derived fragments more than one model task
-- composes with. 'petLine' feeds both 'petBrief' and the weekly recap, so a pet is described
-- one way everywhere. 'householdVisionNote' steers the identification guide and the vision
-- prompt alike. Task-specific prompt text lives in each task's own module.
module PetReport.Analysis.Vocabulary
  ( petBrief
  , petLine
  , householdVisionNote
  ) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           PetReport.Domain.Profile (Household (..), Pet (..), Profile (..),
                                           activePets)
import           PetReport.Domain.Types   (speciesText)

-- | The raw roster brief: one 'petLine' per active pet, straight from the profile. Feeds
-- the 'PetReport.Analysis.IdentGuide' meta-prompt, and is the fallback every prompt consumer
-- gets when the curated brief cache is missing or stale.
petBrief :: Profile -> Text
petBrief prof = case activePets prof of
  [] -> "No specific pets are configured yet; describe animals by species."
  ps -> "Household pets:\n" <> T.intercalate "\n" ["- " <> petLine p | p <- ps]

-- | One line describing a pet for the model: name, species, description, plus any owner
-- note. Both 'petBrief' and the weekly-recap system prompt build from here.
petLine :: Pet -> Text
petLine p =
  petName p
    <> " ("
    <> speciesText (petSpecies p)
    <> "): "
    <> petDescription p
    <> maybe "" (" Note: " <>) (petNotes p)

-- | Identification-relevant household context, a cat flap or a neighbour's cat, appended to
-- the identification brief so the vision model sees it and not just the daily narrative.
-- Empty when neither applies.
householdVisionNote :: Household -> Text
householdVisionNote h =
  T.intercalate "\n" $
    ["A neighbour's cat visits, so an unfamiliar cat may be a visitor rather than one of the household pets above; when it does not clearly match a pet, treat it as a visiting cat and lower the confidence." | neighbourCat h]
      ++ ["There is a cat flap, so a pet coming in from outside or heading out is normal, not a pet gone missing." | catFlap h]

