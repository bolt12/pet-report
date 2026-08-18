-- | The user-owned profile: the pet roster, report preferences, household context, and the
-- camera-to-room map. Set up on first run and editable any time. This is app state
-- persisted in the database, not infrastructure config; it steers the prompts and drives
-- the per-pet UI.
--
-- Identity (which pet) is /derived/ from the roster here, never stored. Decoding is
-- tolerant, with every field defaulted, so a shape change never hard-fails a stored profile.
module PetReport.Domain.Profile
  ( Pet (..)
  , Roster
  , activePets
  , ReportTopic (..)
  , ReportPrefs (..)
  , Household (..)
  , emptyHousehold
  , CameraRoom (..)
  , Profile (..)
  , emptyProfile
  , enabledCameras
  , roomOf
  , Identity (..)
  , identify
  , SubjectId (..)
  , Overrides
  , identifyWith
  , uniquePetOfSpecies
  , petById
  , CorrectionTarget (..)
  , resolveCorrection
  ) where

import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              genericParseJSON, genericToJSON,
                                              withObject, (.!=), (.:?))
import           Data.Char                   (toUpper)
import           Data.Int                    (Int64)
import           Data.List                   (find)
import           Data.Maybe                  (isNothing)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime)
import           GHC.Generics                (Generic)
import           PetReport.Domain.Perception (Appearance (..), Correction (..),
                                              Who (..))
import           PetReport.Domain.Types      (Camera, PetId (..), Species (..),
                                              cameraText, enumOptions)

data Pet = Pet
  { petId          :: PetId
  , petName        :: Text
  , petSpecies     :: Species
  , petDescription :: Text
  -- ^ Visual identity for the model, e.g. "orange short-haired cat, green collar".
  , petNotes       :: Maybe Text
  -- ^ Caveats, e.g. "three legs; normal, not an injury".
  , petArchivedAt  :: Maybe UTCTime
  -- ^ When the owner archived the pet. Archived pets are hidden and excluded from
  -- auto-identification, but their data is kept and their existing corrections still
  -- resolve to their name. 'Nothing' means active, which is also how a missing key decodes.
  , petPhoto       :: Maybe Text
  -- ^ Short content hash of the pet's avatar photo; the JPEG bytes live on disk in the
  -- media dir, keyed by pet id. Signals presence and cache-busts the served URL. 'Nothing'
  -- means no photo, which is also how a missing key decodes.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Roster = [Pet]

-- | Topics the owner wants the daily summary to emphasise; they map to tracked
-- facts and report sections.
data ReportTopic
  = Meals
  | Water
  | Litter
  | Sleep
  | Play
  | Grooming
  | Visitors
  | Sounds
  | Outdoors
  | Health
  | UnusualBehaviour
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON ReportTopic where
  toJSON = genericToJSON enumOptions

instance FromJSON ReportTopic where
  parseJSON = genericParseJSON enumOptions

data ReportPrefs = ReportPrefs
  { topics   :: Set ReportTopic
  , freeform :: Maybe Text
  -- ^ "Anything else you especially want to know."
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ReportPrefs where
  parseJSON = withObject "ReportPrefs" $ \o ->
    ReportPrefs
      <$> o .:? "topics" .!= Set.empty
      <*> o .:? "freeform"

-- | Structured household context; steers the narrative and grounds the model
-- (a cat flap explains outdoor trips; a neighbour's cat explains a stranger).
data Household = Household
  { catFlap      :: Bool
  , neighbourCat :: Bool
  , feedTimes    :: Maybe Text
  , notes        :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Household where
  parseJSON = withObject "Household" $ \o ->
    Household
      <$> o .:? "catFlap" .!= False
      <*> o .:? "neighbourCat" .!= False
      <*> o .:? "feedTimes"
      <*> o .:? "notes"

emptyHousehold :: Household
emptyHousehold = Household False False Nothing Nothing

-- | A Frigate camera: its name (the id pet-report pulls frames by), the friendly
-- room label shown in the UI, and whether it is enabled for monitoring.
data CameraRoom = CameraRoom
  { camId   :: Text
  , room    :: Text
  , enabled :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON CameraRoom where
  parseJSON = withObject "CameraRoom" $ \o ->
    CameraRoom
      <$> o .:? "camId" .!= ""
      <*> o .:? "room" .!= ""
      <*> o .:? "enabled" .!= True

data Profile = Profile
  { pets         :: [Pet]
  , report       :: ReportPrefs
  , household    :: Household
  , cameras      :: [CameraRoom]
  -- ^ The Frigate cameras (name + room + enabled), editable in setup.
  , frigateUrl   :: Maybe Text
  -- ^ Overrides the env Frigate URL when set.
  , modelUrl     :: Maybe Text
  -- ^ Overrides the env vision-model (llama-swap) URL when set.
  , visionModel  :: Maybe Text
  -- ^ Overrides the env vision-model /name/ when set.
  , timeZone     :: Maybe Text
  -- ^ The owner's IANA zone (e.g. @America/New_York@), captured in setup. Steers day
  -- boundaries and the morning/evening split. Overrides the @PET_REPORT_TZ@ env default;
  -- blank falls back to it.
  , gcWindowDays :: Int
  -- ^ How many days of raw moments to keep before garbage-collecting the un-kept ones.
  -- Their durable per-day stats and any kept moments remain. Honoured literally at a
  -- one-day minimum, so shortening it actually takes effect. Default 30.
  , captureSecs  :: Maybe Int
  -- ^ Seconds between capture passes, overriding the @PET_REPORT_CAPTURE_SECS@ env default
  -- when set. How often a camera Frigate has been quiet on gets a blind sample, so it trades
  -- coverage of an uneventful stretch against the number of near-identical moments to page
  -- through. 'Nothing' falls back to the env value.
  , configuredAt :: Maybe UTCTime
  -- ^ 'Nothing' until the owner completes the setup wizard.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \o ->
    Profile
      <$> o .:? "pets" .!= []
      <*> o .:? "report" .!= ReportPrefs Set.empty Nothing
      <*> o .:? "household" .!= emptyHousehold
      <*> o .:? "cameras" .!= []
      <*> o .:? "frigateUrl"
      <*> o .:? "modelUrl"
      <*> o .:? "visionModel"
      <*> o .:? "timeZone"
      <*> o .:? "gcWindowDays" .!= 30
      <*> o .:? "captureSecs"
      <*> o .:? "configuredAt"

emptyProfile :: Profile
emptyProfile =
  Profile
    { pets = []
    , report = ReportPrefs {topics = Set.empty, freeform = Nothing}
    , household = emptyHousehold
    , cameras = []
    , frigateUrl = Nothing
    , modelUrl = Nothing
    , visionModel = Nothing
    , timeZone = Nothing
    , gcWindowDays = 30
    , captureSecs = Nothing
    , configuredAt = Nothing
    }

-- | The names of the enabled cameras (empty if none configured).
enabledCameras :: Profile -> [Text]
enabledCameras = map camId . filter enabled . cameras

-- | The friendly room label for a camera. Falls back to a title-cased id when the
-- camera is not in the map or its room label is blank (e.g. @living_room@ becomes
-- @Living Room@).
roomOf :: [CameraRoom] -> Camera -> Text
roomOf crs cam =
  let cid = cameraText cam
   in case find ((== cid) . camId) crs of
        Just cr | not (T.null (T.strip (room cr))) -> room cr
        _                                          -> titleCase cid

titleCase :: Text -> Text
titleCase = T.unwords . map cap . T.words . T.map underToSpace
  where
    underToSpace c = if c == '_' then ' ' else c
    cap w = case T.uncons w of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> w

-- | Who a subject actually is, resolved against the roster. 'Visiting' is an
-- animal the owner marked as not theirs (the neighbour's cat through the flap);
-- it is a distinct identity so it never lands in a pet's or a species' stats.
data Identity
  = KnownPet Pet
  | UnknownAnimal Species
  | Visiting Species
  | Human
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Resolve an appearance to an identity. A species maps to a roster pet only when exactly
-- one pet has that species; zero or several leaves it an 'UnknownAnimal', which also feeds
-- the mis-ID caveat. This is the no-override case of 'identifyWith'.
identify :: Roster -> Appearance -> Identity
identify roster ap = case who ap of
  APerson     -> Human
  AnAnimal sp -> maybe (UnknownAnimal sp) KnownPet (uniquePetOfSpecies roster sp)

-- | An owner-assigned override target for one appearance: a specific pet, or a
-- visiting (not-mine) animal. Stored in @subject_identity@ and read back into
-- 'Overrides'; the source of truth for individual identity.
data SubjectId
  = IdPet PetId
  | IdVisiting
  deriving stock (Eq, Show)

-- | Owner identity overrides for a query window, keyed by @(obs_id, seq)@ (the
-- same 0-based appearance index the facts projection uses). Empty when there are
-- no corrections, in which case 'identifyWith' is exactly 'identify'.
type Overrides = Map (Int64, Int) SubjectId

-- | Resolve an appearance to an identity, consulting owner overrides first and the roster's
-- unique-species rule second. Individual identity is decided here and nowhere else. A stale
-- pet id, removed from the roster, falls back to 'identify' rather than naming a pet that
-- no longer exists.
identifyWith :: Overrides -> Roster -> (Int64, Int) -> Appearance -> Identity
identifyWith ov roster key ap = case Map.lookup key ov of
  Just (IdPet pid) -> maybe (identify roster ap) KnownPet (petById roster pid)
  Just IdVisiting -> case who ap of
    AnAnimal sp -> Visiting sp
    APerson     -> Human
  Nothing -> identify roster ap

-- | The roster pet of a species when exactly one ACTIVE pet has it. Both 'identify' and the
-- long-window per-pet stats attribution resolve species through here, so one sighting counts
-- for the same pet on a card and in a month total.
--
-- Archived pets are excluded, so a deceased pet no longer auto-claims sightings. Its
-- existing corrections still resolve by id through 'petById'.
uniquePetOfSpecies :: Roster -> Species -> Maybe Pet
uniquePetOfSpecies roster sp =
  case filter (\p -> petSpecies p == sp && isNothing (petArchivedAt p)) roster of
    [p] -> Just p
    _   -> Nothing

-- | Find a roster pet by id (including archived pets, so an old correction still
-- resolves to a deceased pet's name).
petById :: Roster -> PetId -> Maybe Pet
petById roster pid = find ((== pid) . petId) roster

-- | The pets currently present (not archived). Use where the roster means "which
-- pets are here now" (tiles, the model prompt, weekly recaps), not for resolving
-- a known id.
activePets :: Profile -> [Pet]
activePets = filter (isNothing . petArchivedAt) . pets

-- | The already-decoded target of an owner correction: exactly one of four things. A sum
-- rather than four nullable fields, so "this is Mochi and also a visitor and also a person"
-- cannot be expressed and then silently resolved by a precedence chain. The web layer
-- decodes its DTO into this, keeping the aeson wrapper out of plain roster logic.
data CorrectionTarget
  = TargetPet Text
  | TargetSpecies Text
  | TargetPerson
  | TargetVisiting
  deriving stock (Eq, Show)

-- | Resolve an owner correction target against the roster. Only 'TargetPet' can fail, and
-- it fails loudly: naming a pet the roster does not hold is a caller error, so it is
-- rejected rather than quietly demoted to the species. That demotion used to turn a typo
-- into a correction the owner never asked for.
resolveCorrection :: Roster -> CorrectionTarget -> Maybe Correction
resolveCorrection roster t = case t of
  TargetVisiting -> Just ToVisiting
  TargetPerson -> Just ToPerson
  TargetSpecies s -> Just (ToSpecies (Species s))
  TargetPet pid
    | any ((== PetId pid) . petId) roster -> Just (ToPet (PetId pid))
    | otherwise -> Nothing
