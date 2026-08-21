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
  , Attribution (..)
  , Certainty (..)
  , Overrides
  , identifyWith
  , activeOfSpecies
  , uniquePetOfSpecies
  , petById
  , petByName
  , namedPet
  , resolvePetNames
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
                                              Scene (..), Who (..),
                                              animalSpecies)
import           PetReport.Domain.Types      (Camera, PetId (..), Species (..),
                                              cameraText, enumOptions,
                                              speciesText)

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
  = KnownPet Pet Certainty
  | UnknownAnimal Species
  | Visiting Species
  | Human
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | How a pet came to be named on a sighting.
--
-- Only the card cares: both count, and 'PetReport.Domain.Stats.keyOf' sends both to the same
-- key, because an attribution the app is willing to act on is one it should also be willing
-- to total. What differs is what the owner is being told, and a screen that shows a machine
-- guess as settled fact has no way to invite the correction that fixes it.
data Certainty
  = Confirmed
  -- ^ The owner said so, or the roster leaves no other option (one active pet of the
  -- species).
  | ByModel
  -- ^ The vision model named this pet and was confident. Shown as unconfirmed.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Resolve an appearance to an identity with no stored attribution to consult: the roster's
-- unique-species rule, and nothing else. Zero or several pets of the species leaves it an
-- 'UnknownAnimal', which also feeds the mis-ID caveat.
--
-- This is deliberately NOT where the model's own naming is read. A name in the blob is a
-- claim, and a claim is resolved once, at storage, into a row 'identifyWith' can weigh; if
-- it were read here as well the same guess would be applied twice by two different rules.
identify :: Roster -> Appearance -> Identity
identify roster ap = case who ap of
  APerson     -> Human
  AnAnimal sp -> maybe (UnknownAnimal sp) (`KnownPet` Confirmed) (uniquePetOfSpecies roster sp)

-- | An assigned identity for one appearance: a specific pet, or a visiting (not-mine)
-- animal. Stored in @subject_identity@ and read back into 'Overrides'; the source of truth
-- for individual identity.
data SubjectId
  = IdPet PetId
  | IdVisiting
  deriving stock (Eq, Show)

-- | One stored attribution, with who made it.
--
-- The owner's word and the model's guess share a table and a resolution path, and differ in
-- exactly one way: what the card says about how sure it is. Carrying that here rather than
-- re-deriving it downstream keeps identity resolved in one place.
data Attribution = Attribution
  { atSubject   :: SubjectId
  , atCertainty :: Certainty
  }
  deriving stock (Eq, Show)

-- | Stored identity attributions for a query window, keyed by @(obs_id, seq)@ (the
-- same 0-based appearance index the facts projection uses). Empty when there are
-- none, in which case 'identifyWith' is exactly 'identify'.
type Overrides = Map (Int64, Int) Attribution

-- | Resolve an appearance to an identity, consulting the stored attribution first and the
-- roster's unique-species rule second. Individual identity is decided here and nowhere else.
-- A stale pet id, removed from the roster, falls back to 'identify' rather than naming a pet
-- that no longer exists.
--
-- An owner row and a model row are both honoured; only their 'Certainty' differs. The two
-- never compete, because a correction overwrites the row it corrects.
identifyWith :: Overrides -> Roster -> (Int64, Int) -> Appearance -> Identity
identifyWith ov roster key ap = case Map.lookup key ov of
  Just (Attribution (IdPet pid) certainty) ->
    maybe (identify roster ap) (\p -> KnownPet p (settle p certainty)) (petById roster pid)
  Just (Attribution IdVisiting _) -> case who ap of
    AnAnimal sp -> Visiting sp
    APerson     -> Human
  Nothing -> identify roster ap
  where
    -- A model naming the ONLY pet of its species has not guessed at anything: the roster
    -- leaves no other answer, and 'identify' would have reached the same pet with no
    -- attribution at all. Marking that as unconfirmed would put "my guess" on every card in
    -- a one-pet household, for an identification the app treats as certain everywhere else.
    settle p ByModel
      | uniquePetOfSpecies roster (petSpecies p) == Just p = Confirmed
    settle _ c = c

-- | The active pets of a species, in roster order. 'uniquePetOfSpecies' is the one-pet
-- question asked of this list; a card naming the candidates it could not choose between asks
-- for the list itself.
activeOfSpecies :: Roster -> Species -> [Pet]
activeOfSpecies roster sp = [p | p <- roster, petSpecies p == sp, isNothing (petArchivedAt p)]

-- | The roster pet of a species when exactly one ACTIVE pet has it. Both 'identify' and the
-- long-window per-pet stats attribution resolve species through here, so one sighting counts
-- for the same pet on a card and in a month total.
--
-- Archived pets are excluded, so a deceased pet no longer auto-claims sightings. Its
-- existing corrections still resolve by id through 'petById'.
uniquePetOfSpecies :: Roster -> Species -> Maybe Pet
uniquePetOfSpecies roster sp = case activeOfSpecies roster sp of
  [p] -> Just p
  _   -> Nothing

-- | Find a roster pet by id (including archived pets, so an old correction still
-- resolves to a deceased pet's name).
petById :: Roster -> PetId -> Maybe Pet
petById roster pid = find ((== pid) . petId) roster

-- | Find a roster pet by the name a model used for it, ignoring case and surrounding space.
--
-- The one definition of what counts as a match. Two readers depend on agreeing: the decode
-- that moves a name into 'PetReport.Domain.Perception.pet', and the write that turns that
-- name into a stored attribution. A second spelling of the normalisation would let them
-- drift, and the drift would be silent, showing up only as a name the app accepted and then
-- could not attribute.
petByName :: Roster -> Text -> Maybe Pet
petByName roster raw
  | T.null k = Nothing
  | otherwise = find ((== k) . petNameKey) roster
  where
    k = petNameKey' raw

-- | The comparison key for a pet's name.
petNameKey :: Pet -> Text
petNameKey = petNameKey' . petName

petNameKey' :: Text -> Text
petNameKey' = T.toCaseFold . T.strip

-- | The roster pet an appearance names, when it names one that holds up.
--
-- The single question "does this sighting claim to be a specific pet", asked by the decode
-- that settles the blob and by the write that turns the claim into a stored attribution.
-- While they asked it separately the write was the looser of the two, and would have given a
-- PERSON a pet identity, which 'PetReport.Effect.Db.Queries.writeOverride' refuses on the
-- owner's path and the stats projection drops on the way out. A card, though, reads through
-- 'identifyWith', which would have believed it.
--
-- Three ways to name nothing: a subject that is not an animal at all, a name no pet answers
-- to, and a name belonging to a pet of some other species.
namedPet :: Roster -> Appearance -> Maybe Pet
namedPet roster ap = do
  nm <- pet ap
  sp <- animalSpecies (who ap)
  p <- petByName roster nm
  if petSpecies p == sp then Just p else Nothing

-- | Move a pet's name out of the species slot and into the one that means it, and drop a
-- name that contradicts the species it arrived with.
--
-- 'Who' reads any token it does not recognise as a species, so before 'pet' existed a model
-- answering @"who": "Yuki"@ minted a species nothing downstream knew about: no @petId@ on
-- the card, invisible to the pet facet (which compares real species), and a second row
-- beside the pet's own in the day's per-pet totals. The name was never noise, though. It is
-- the only thing that can tell two dogs apart, so it is kept, in the field that means it.
--
-- A token that already names a roster species wins, so a cat called Dog keeps genuine dog
-- sightings as dogs. A pet named after some OTHER species (a cat called Rabbit) still claims
-- that word, which no amount of matching can separate from a real rabbit.
--
-- Idempotent: an appearance already carrying a 'pet' is left alone unless its species
-- disagrees.
resolvePetNames :: Roster -> Scene -> Scene
resolvePetNames roster sc = sc {appearances = map fixup (appearances sc)}
  where
    fixup ap = case who ap of
      -- A person is never a pet, whatever the model attached to it.
      APerson -> ap {pet = Nothing}
      AnAnimal sp
        -- The name arrived in the species slot: move it, and settle the species.
        | not (petNameKey' (speciesText sp) `Set.member` rosterSpecies)
        , Just p <- petByName roster (speciesText sp) ->
            ap {who = AnAnimal (petSpecies p), pet = Just (petName p)}
        -- A name in its own field stands only when the species agrees with it. A model that
        -- calls the same subject a cat and Yuki the dog has identified nothing, and taking
        -- either half would be inventing the answer.
        | otherwise -> ap {pet = petName <$> namedPet roster ap}
    rosterSpecies = Set.fromList [petNameKey' (speciesText (petSpecies p)) | p <- roster]

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
  -- The roster is what knows the pet's species, so it is attached here rather than left
  -- for the persistence layer to look up.
  TargetPet pid -> case find ((== PetId pid) . petId) roster of
    Just p  -> Just (ToPet (petId p) (petSpecies p))
    Nothing -> Nothing
