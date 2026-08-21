-- | Pure derivations over observations. Two groups: per-subject aggregation (sightings,
-- rest/active split, behaviour counts, keyed by resolved identity, exact per-pet because
-- each appearance is credited to its own subject), and the per-moment signals a card and
-- the derived @observations@ columns both read: 'wellbeingOf', 'confidenceOf',
-- 'isUncertain', 'isConcerning' and 'needsLook'.
--
-- The signals live here rather than beside their readers so a rule has one definition. When
-- the card and the stored column each carried their own copy, they drifted.
module PetReport.Domain.Stats
  ( SubjectKey (..)
  , ResolvedStats (..)
  , StoredStats (..)
  , keyOf
  , PetStat (..)
  , emptyPetStat
  , statOf
  , appearancesOf
  , identifiedAppearances
  , subjectAppearances
  , presence
  , Presence (..)
  , homePresence
  , SubjectFact (..)
  , factsOf
  , wellbeingOf
  , confidenceOf
  , concerningAppearance
  , isUncertain
  , isConcerning
  , needsLook
  ) where

import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import           PetReport.Domain.Behavior    (Behaviors (..),
                                               accidentSuspected,
                                               injurySuspected)
import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.Perception  (Appearance (..), Perception (..),
                                               Scene (..), animalSpecies, isPerson,
                                               isSafetySound, sceneAppearances)
import           PetReport.Domain.Profile     (Identity (..), Overrides, Pet (..),
                                               Roster, identifyWith)
import           PetReport.Domain.Types       (Activity (..), ObsId (..), PetId,
                                               Species, Wellbeing (..),
                                               activityText, confidenceValue,
                                               speciesText)

-- | A stable, orderable key for one subject. 'KVisitor' is a not-mine animal;
-- it is distinct from 'KPet' and 'KSpecies' so visitors never enter per-pet or
-- per-species stats.
data SubjectKey
  = KPet PetId
  | KSpecies Species
  | KVisitor Species
  | KPerson
  deriving stock (Eq, Ord, Show)

-- | Per-subject stats with identity ALREADY resolved against the roster: a lone cat's
-- sightings sit under 'KPet', and 'KSpecies' holds only genuinely ambiguous ones.
newtype ResolvedStats = ResolvedStats {resolvedStatsMap :: Map SubjectKey PetStat}
  deriving stock (Eq, Show)

-- | Per-subject stats exactly as the projection stores them: species-level unless the
-- owner overrode a sighting, so a lone pet's sightings sit under 'KSpecies' and have to be
-- folded in by 'PetReport.Domain.PetReport.petStatFor' before they can be read per pet.
--
-- Distinct from 'ResolvedStats' because the two are NOT interchangeable and used to share
-- one type, so nothing stopped a caller reading a stored map as though it were resolved.
-- One did: the day-stats endpoint labelled a lone pet's sightings with its species while
-- every other surface named the pet.
newtype StoredStats = StoredStats {storedStatsMap :: Map SubjectKey PetStat}
  deriving stock (Eq, Show)

-- Certainty is deliberately ignored: a sighting the app is willing to attribute to a pet is
-- one it should also be willing to total for that pet, or the number under a name would
-- disagree with the cards carrying it.
keyOf :: Identity -> SubjectKey
keyOf i = case i of
  KnownPet p _     -> KPet (petId p)
  UnknownAnimal sp -> KSpecies sp
  Visiting sp      -> KVisitor sp
  Human            -> KPerson

-- | Behaviour counts for one subject: each field is the number of appearances in
-- which that fact fired ('statOf' is the single-appearance contribution). Combined
-- field-wise by '<>', so stats compose across appearances, days, and windows.
data PetStat = PetStat
  { psSightings  :: !Int
  , psRest       :: !Int
  , psActive     :: !Int
  , psAte        :: !Int
  , psDrank      :: !Int
  , psSlept      :: !Int
  , psPlayed     :: !Int
  , psGroomed    :: !Int
  , psEliminated :: !Int
  , psConcerns   :: !Int
  }
  deriving stock (Eq, Show)

emptyPetStat :: PetStat
emptyPetStat = PetStat 0 0 0 0 0 0 0 0 0 0

instance Semigroup PetStat where
  a <> b =
    PetStat
      { psSightings = psSightings a + psSightings b
      , psRest = psRest a + psRest b
      , psActive = psActive a + psActive b
      , psAte = psAte a + psAte b
      , psDrank = psDrank a + psDrank b
      , psSlept = psSlept a + psSlept b
      , psPlayed = psPlayed a + psPlayed b
      , psGroomed = psGroomed a + psGroomed b
      , psEliminated = psEliminated a + psEliminated b
      , psConcerns = psConcerns a + psConcerns b
      }

instance Monoid PetStat where
  mempty = emptyPetStat

restActs :: [Activity]
restActs = [Sleeping, Resting, Sitting]

activeActs :: [Activity]
activeActs = [Walking, Running, Jumping, Playing, Alert, Grooming, Standing]

-- | The one-appearance contribution to a subject's stats.
statOf :: Appearance -> PetStat
statOf ap =
  let b = behaviors ap
      act = activity ap
      ind x = if x then 1 else 0
   in PetStat
        { psSightings = 1
        , psRest = ind (act `elem` restActs || slept b)
        , psActive = ind (act `elem` activeActs || played b)
        , psAte = ind (ate b)
        , psDrank = ind (drank b)
        , psSlept = ind (slept b)
        , psPlayed = ind (played b)
        , psGroomed = ind (groomed b)
        , psEliminated = ind (eliminatedHere b)
        , psConcerns = ind (concerningAppearance ap)
        }
  where
    eliminatedHere bs = case eliminated bs of
      Just _  -> True
      Nothing -> False

-- | The queryable projection of one appearance: the flat behaviour facts, plus the raw
-- species or person and the scene confidence. The facts come from 'statOf', so a stored row
-- counts a fact exactly as the domain does. One 'SubjectFact' becomes one
-- @observation_subjects@ row.
data SubjectFact = SubjectFact
  { sfSpecies    :: Maybe Text
  , sfIsPerson   :: Bool
  , sfActivity   :: Maybe Text
  , sfRest       :: Bool
  , sfActive     :: Bool
  , sfAte        :: Bool
  , sfDrank      :: Bool
  , sfSlept      :: Bool
  , sfPlayed     :: Bool
  , sfGroomed    :: Bool
  , sfEliminated :: Bool
  , sfAccident   :: Bool
  , sfConcern    :: Bool
  , sfConfidence :: Maybe Double
  }
  deriving stock (Eq, Show)

-- | Project a perception into its per-appearance facts (empty for a sound).
factsOf :: Perception -> [SubjectFact]
factsOf (Seen sc) = map mk (appearances sc)
  where
    conf = confidenceValue <$> confidence sc
    mk ap =
      let st = statOf ap
          -- Every fact is "this stat fired for the appearance", so read them all off the
          -- one 'PetStat' instead of re-spelling @> 0@ per field.
          fired f = f st > 0
       in SubjectFact
            { sfSpecies = speciesText <$> animalSpecies (who ap)
            , sfIsPerson = isPerson (who ap)
            , sfActivity = Just (activityText (activity ap))
            , sfRest = fired psRest
            , sfActive = fired psActive
            , sfAte = fired psAte
            , sfDrank = fired psDrank
            , sfSlept = fired psSlept
            , sfPlayed = fired psPlayed
            , sfGroomed = fired psGroomed
            , sfEliminated = fired psEliminated
            , sfAccident = accidentSuspected (behaviors ap)
            , sfConcern = fired psConcerns
            , sfConfidence = conf
            }
factsOf (Heard _) = []

-- | The wellbeing a moment's card carries: a scene reports its own, a safety sound reads as
-- 'Concerning', and any other sound has nothing to say.
--
-- One rule, two readers: the card's label and 'isConcerning' behind the review backlog.
-- Spelling it out separately in each is how the "N to check" badge came to count moments the
-- backlog did not hold.
wellbeingOf :: Perception -> Maybe Wellbeing
wellbeingOf (Seen sc)  = Just (wellbeing sc)
wellbeingOf (Heard sk) = if isSafetySound sk then Just Concerning else Nothing

-- | The confidence a moment carries: a scene's own, and none for a sound.
--
-- Read by the card and by the @observations.confidence@ column alike. It takes the scene's
-- own value rather than the first projected fact, so an empty room keeps the confidence the
-- model gave it: 'factsOf' has no row to carry one when nothing was visible.
confidenceOf :: Perception -> Maybe Double
confidenceOf (Seen sc) = confidenceValue <$> confidence sc
confidenceOf (Heard _) = Nothing

-- | Whether the model was unsure of a moment: it said so outright, or its confidence sits
-- below the review threshold. A sound is never uncertain, carrying neither.
--
-- This is only the card's "not fully sure" mark. What reaches the review backlog is
-- 'needsLook', which is wider.
isUncertain :: Perception -> Bool
isUncertain p = wellbeingOf p == Just Unsure || maybe False (< 0.62) (confidenceOf p)

-- | Whether one appearance carries a health signal worth a look: a recorded concern, a
-- suspected injury, or an indoor accident.
--
-- Counted per appearance by 'statOf' and read whole-moment by
-- 'PetReport.Pipeline.concerningObs', which is why it is named rather than spelled out at
-- each site.
concerningAppearance :: Appearance -> Bool
concerningAppearance ap =
  let b = behaviors ap
   in not (null (concerns b)) || accidentSuspected b || injurySuspected b

-- | Flagged concerning, by the same rule that labels the card.
--
-- Narrower than 'PetReport.Pipeline.concerningObs', which also counts per-appearance health
-- signals when deciding whether a whole day earns an alert.
isConcerning :: Perception -> Bool
isConcerning p = wellbeingOf p == Just Concerning

-- | The needs-a-look rule: the model was unsure, or the moment is flagged concerning. Two
-- separate reasons to look, and a safety sound shows why both are needed: it carries no
-- confidence to be unsure about, yet it is the thing an owner most wants surfaced.
--
-- Both the card ('PetReport.Domain.View.ovNeedsReview') and the @observations.needs_look@
-- column read this one rule, so a moment queued in the column is queued on the card.
needsLook :: Perception -> Bool
needsLook p = isUncertain p || isConcerning p

-- | The visible appearances of an observation (empty for audio).
appearancesOf :: Observation -> [Appearance]
appearancesOf = sceneAppearances . perception

-- | Resolve every appearance of an observation to its @(Identity, Appearance)@, consulting
-- owner overrides keyed by @(obs_id, seq)@. The seq addressing happens once here, so callers
-- iterate resolved pairs rather than re-deriving it.
identifiedAppearances :: Overrides -> Roster -> Observation -> [(Identity, Appearance)]
identifiedAppearances ov roster obs =
  [ (identifyWith ov roster (oid, s) ap, ap)
  | (s, ap) <- zip [0 ..] (appearancesOf obs)
  ]
  where
    ObsId oid = obsId obs

-- | The appearances of an observation attributed to one subject, identity resolved with
-- overrides. Backs the per-subject histograms, last-seen, and \"does this pet appear here\"
-- checks.
subjectAppearances :: Overrides -> Roster -> SubjectKey -> Observation -> [Appearance]
subjectAppearances ov roster key obs =
  [ap | (idn, ap) <- identifiedAppearances ov roster obs, keyOf idn == key]

-- | Per-subject stats over a set of observations, honouring owner overrides.
presence :: Overrides -> Roster -> [Observation] -> ResolvedStats
presence ov roster obss =
  ResolvedStats $
  Map.fromListWith
    (<>)
    [ (keyOf idn, statOf ap)
    | obs <- obss
    , (idn, ap) <- identifiedAppearances ov roster obs
    ]

-- | Coarse human-presence over a window: whether any person was seen, when last,
-- and how many person sightings. Derived only from person appearances (no face
-- recognition), so it answers "was someone home?" without ever claiming /who/.
data Presence = Presence
  { prSomeoneHome     :: Bool
  , prLastPersonAt    :: Maybe UTCTime
  , prPersonSightings :: Int
  }
  deriving stock (Eq, Show)

homePresence :: [Observation] -> Presence
homePresence obss =
  case [at o | o <- obss, ap <- appearancesOf o, isPerson (who ap)] of
    []    -> Presence False Nothing 0
    times -> Presence True (Just (maximum times)) (length times)
