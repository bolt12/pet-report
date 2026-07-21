-- | Pure per-subject aggregation over observations: sightings, rest/active
-- split, and behaviour counts, keyed by resolved identity. Exact per-pet
-- attribution because each appearance is credited to its own subject.
module PetReport.Domain.Stats
  ( SubjectKey (..)
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
  , isUncertain
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
                                               Scene (..), animalSpecies, isPerson)
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

keyOf :: Identity -> SubjectKey
keyOf i = case i of
  KnownPet p       -> KPet (petId p)
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
        , psConcerns = ind (not (null (concerns b)) || accidentSuspected b || injurySuspected b)
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

-- | The needs-a-look signal: an unsure scene, or one whose confidence sits below the review
-- threshold. A sound is never uncertain, carrying neither wellbeing nor confidence.
--
-- Both the presentation view ('PetReport.Domain.View.viewOf') and the
-- @observations.uncertain@ column read this one rule, so a moment flagged in the column is
-- flagged on the card.
isUncertain :: Perception -> Bool
isUncertain (Seen sc) =
  wellbeing sc == Unsure || maybe False ((< 0.62) . confidenceValue) (confidence sc)
isUncertain (Heard _) = False

-- | The visible appearances of an observation (empty for audio).
appearancesOf :: Observation -> [Appearance]
appearancesOf obs = case perception obs of
  Seen sc -> appearances sc
  Heard _ -> []

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
presence :: Overrides -> Roster -> [Observation] -> Map SubjectKey PetStat
presence ov roster obss =
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
