{-# LANGUAGE DataKinds #-}

-- | The HTTP wire types: request and response DTOs with their JSON instances, the small pure
-- converters between a DTO and its domain shape, and the raw image and video content types.
-- Keeping them here leaves the handler modules readable as handlers, rather than handlers
-- interleaved with aeson boilerplate.
module PetReport.Web.Types
  ( DayResponse (..)
  , DayStat (..)
  , PresenceV (..)
  , presenceV
  , CameraInfo (..)
  , AskReq (..)
  , AskResp (..)
  , RefreshResp (..)
  , ReviewReq (..)
  , RecapReq (..)
  , DeleteMomentsReq (..)
  , RefreshReq (..)
  , OkResp (..)
  , RecapResp (..)
  , CorrectReq (..)
  , correctionTarget
  , correctionKinds
  , AddSightingReq (..)
  , addedSighting
  , sightingKinds
  , EditReq (..)
  , KeepsakeReq (..)
  , AddPetReq (..)
  , EditPetReq (..)
  , applyPetEdit
  , DescribeReq (..)
  , DescribeResp (..)
  , JPEG
  , MP4
  , Cached
  , cached
  ) where

import           Data.Aeson           (FromJSON (..), ToJSON (..),
                                       genericToJSON, object, withObject,
                                       (.!=), (.:), (.:?), (.=))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Control.Applicative  ((<|>))
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import qualified Network.HTTP.Media   as M
import           Servant              (Accept (..), Header, Headers,
                                       MimeRender (..), addHeader)

import           PetReport.Domain.Perception (SceneEdit (..), Who (..))
import           PetReport.Domain.Profile    (CorrectionTarget (..), Pet (..))
import           PetReport.Domain.Stats      (Presence (..))
import           PetReport.Domain.Types      (PetId (..), Species (..))
import           PetReport.Domain.View       (ObsView)
import           PetReport.Util              (prefixed)

data DayResponse = DayResponse
  { day          :: Text
  , observations :: [ObsView]
  , narrative    :: Maybe Text
  , presence     :: PresenceV
  , dayStats     :: [DayStat]
  -- ^ Per-pet stats for the day: a live compute over the day's moments while any remain,
  -- otherwise the durable rollup. This is what keeps an old day meaningful once its raw
  -- moments are gone, retired pets included.
  }
  deriving stock (Generic)

-- Emits the contract Day model field names. The internal record field names differ to avoid
-- clashing with the domain 'pets' and 'narrative' accessors already in scope here.
instance ToJSON DayResponse where
  toJSON dr =
    object
      [ "date" .= day dr
      , "narrative" .= narrative dr
      , "presence" .= presence dr
      , "pets" .= dayStats dr
      , "moments" .= observations dr
      ]

-- | A day's stats for one pet, or for an unattributed species: the label, the species, and
-- the behaviour counts. Labels resolve via the full roster, so an archived pet keeps its
-- name.
data DayStat = DayStat
  { dsLabel      :: Text
  , dsPetId      :: Maybe Text
  , dsSpecies    :: Text
  , dsSightings  :: Int
  , dsRest       :: Int
  , dsActive     :: Int
  , dsAte        :: Int
  , dsDrank      :: Int
  , dsSlept      :: Int
  , dsPlayed     :: Int
  , dsGroomed    :: Int
  , dsEliminated :: Int
  , dsConcerns   :: Int
  }
  deriving stock (Generic)

instance ToJSON DayStat where
  toJSON = genericToJSON (prefixed 2)

-- | Coarse human presence for the day, so the client can soften its copy to "someone was
-- home this afternoon" without ever claiming who.
data PresenceV = PresenceV
  { someoneHome     :: Bool
  , personSightings :: Int
  , lastPersonAt    :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

presenceV :: Presence -> PresenceV
presenceV p = PresenceV (prSomeoneHome p) (prPersonSightings p) (prLastPersonAt p)

-- | A camera Frigate has configured, with its live online status, for setup
-- auto-discovery.
data CameraInfo = CameraInfo
  { name   :: Text
  , online :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype AskReq = AskReq {question :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data AskResp = AskResp
  { answer :: Text
  , refs   :: [ObsView]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype RefreshResp = RefreshResp {started :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ReviewReq = ReviewReq {ids :: [Int]}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype RecapReq = RecapReq {hours :: Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Plain newtypes with hand-written decoders so a field name does not collide with
-- an accessor already in scope.
newtype DeleteMomentsReq = DeleteMomentsReq (Maybe Text)
instance FromJSON DeleteMomentsReq where
  parseJSON = withObject "DeleteMomentsReq" $ \o -> DeleteMomentsReq <$> o .:? "before"

-- | @{day}@ is optional. Absent or null means today, running the full batch; a past local
-- date rebuilds that day. Tolerant, so a body with no @day@ at all still decodes.
newtype RefreshReq = RefreshReq (Maybe Text)
instance FromJSON RefreshReq where
  parseJSON = withObject "RefreshReq" $ \o -> RefreshReq <$> o .:? "day"

newtype OkResp = OkResp {ok :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype RecapResp = RecapResp {recap :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | An owner correction of one mis-identified sighting. Tagged by @kind@, so the body can
-- carry exactly one target and no combination of flags needs resolving by precedence. The
-- moment id and the sighting index both come from the request path.
newtype CorrectReq = CorrectReq CorrectionTarget

instance FromJSON CorrectReq where
  parseJSON = withObject "CorrectReq" $ \o -> do
    kind <- o .: "kind"
    CorrectReq <$> case kind :: Text of
      "pet"     -> TargetPet <$> o .: "petId"
      "species" -> TargetSpecies <$> o .: "species"
      "person"  -> pure TargetPerson
      "visiting" -> pure TargetVisiting
      other     -> fail (correctKindError other)

-- | The rejection an unrecognised @kind@ earns, naming the legal set so a client author
-- reads the answer rather than guessing. Shared with the vocabulary test.
correctKindError :: Text -> String
correctKindError other =
  "unrecognised correction kind "
    <> show other
    <> "; expected one of "
    <> T.unpack (T.intercalate ", " correctionKinds)

-- | The complete @kind@ vocabulary a correction body may carry.
correctionKinds :: [Text]
-- @visiting@ names an animal that is not the owner's, and @person@ names a human. They
-- used to be @visitor@ and @person@, which read as near-synonyms.
correctionKinds = ["pet", "species", "person", "visiting"]

-- | Hand the decoded target to the roster resolver, keeping the aeson wrapper out of the
-- domain.
correctionTarget :: CorrectReq -> CorrectionTarget
correctionTarget (CorrectReq t) = t

-- | A subject the owner says was present but the model did not report: @{ kind: "person" }@
-- or @{ kind: "species", species: "cat" }@. Only what a camera can perceive, so there is no
-- pet id here; naming the individual is a separate correction against the new sighting.
newtype AddSightingReq = AddSightingReq Who

instance FromJSON AddSightingReq where
  parseJSON = withObject "AddSightingReq" $ \o -> do
    kind <- o .: "kind"
    AddSightingReq <$> case kind :: Text of
      "person"  -> pure APerson
      "species" -> AnAnimal . Species <$> o .: "species"
      other ->
        fail
          ( "unrecognised sighting kind "
              <> show other
              <> "; expected one of "
              <> T.unpack (T.intercalate ", " sightingKinds)
          )

-- | The complete @kind@ vocabulary an added sighting may carry.
sightingKinds :: [Text]
sightingKinds = ["person", "species"]

-- | The perceived subject an add-sighting request names.
addedSighting :: AddSightingReq -> Who
addedSighting (AddSightingReq w) = w

-- | An owner field edit of a moment, as a flat all-optional scene edit. The moment id comes
-- from the request path.
newtype EditReq = EditReq SceneEdit

instance FromJSON EditReq where
  parseJSON = withObject "EditReq" $ \o ->
    EditReq
      <$> ( SceneEdit
              <$> o .:? "activity"
              <*> o .:? "wellbeing"
              <*> o .:? "description"
              <*> o .:? "whereAt"
              <*> o .:? "ate"
              <*> o .:? "drank"
              <*> o .:? "slept"
              <*> o .:? "played"
              <*> o .:? "groomed"
          )

-- | Keep a moment (the moment id is in the path); the body carries only the optional
-- filing pet and caption.
data KeepsakeReq = KeepsakeReq
  { krPetId   :: Maybe Text
  , krCaption :: Maybe Text
  }

instance FromJSON KeepsakeReq where
  parseJSON = withObject "KeepsakeReq" $ \o ->
    KeepsakeReq
      <$> o .:? "petId"
      <*> o .:? "caption"

-- | Add a pet. The body maps the contract's @{id,name,species,description,notes?}@ onto a
-- fresh 'Pet', active and photoless, plus an optional base64 @photo@ that the handler stores
-- on disk and records a content token for.
data AddPetReq = AddPetReq Pet (Maybe Text)

instance FromJSON AddPetReq where
  parseJSON = withObject "AddPetReq" $ \o ->
    AddPetReq
      <$> ( Pet . PetId
              <$> o .: "id"
              <*> o .: "name"
              <*> (Species <$> o .: "species")
              <*> o .: "description"
              <*> o .:? "notes"
              <*> pure Nothing
              <*> pure Nothing
          )
      <*> o .:? "photo"

-- | A partial pet edit: a field that is present replaces the current value, an absent one
-- keeps it. A base64 @photo@ sets or replaces the avatar and @photoRemove@ clears it. The
-- handler sets the avatar token, not 'applyPetEdit'.
data EditPetReq = EditPetReq
  { epName        :: Maybe Text
  , epSpecies     :: Maybe Text
  , epDescription :: Maybe Text
  , epNotes       :: Maybe Text
  , epPhoto       :: Maybe Text
  , epPhotoRemove :: Bool
  }

instance FromJSON EditPetReq where
  parseJSON = withObject "EditPetReq" $ \o ->
    EditPetReq
      <$> o .:? "name"
      <*> o .:? "species"
      <*> o .:? "description"
      <*> o .:? "notes"
      <*> o .:? "photo"
      <*> o .:? "photoRemove" .!= False

applyPetEdit :: EditPetReq -> Pet -> Pet
applyPetEdit req p =
  p
    { petName = fromMaybe (petName p) (epName req)
    , petSpecies = maybe (petSpecies p) Species (epSpecies req)
    , petDescription = fromMaybe (petDescription p) (epDescription req)
    , petNotes = epNotes req <|> petNotes p
    }

-- | Draft a physical description from a single photo: raw base64 @photo@, declared
-- @species@, and the owner's current draft to enhance if there is one. Stateless, carrying
-- no pet id, so it works during first-run onboarding before the pet exists.
data DescribeReq = DescribeReq Text Text (Maybe Text)

instance FromJSON DescribeReq where
  parseJSON = withObject "DescribeReq" $ \o ->
    DescribeReq
      <$> o .: "photo"
      <*> o .:? "species" .!= ""
      <*> o .:? "description"

newtype DescribeResp = DescribeResp Text

instance ToJSON DescribeResp where
  toJSON (DescribeResp d) = object ["description" .= d]

-- A raw JPEG content type for the image endpoints.
data JPEG

instance Accept JPEG where
  contentType _ = "image" M.// "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender _ = LBS.fromStrict

-- A raw MP4 content type for the event-clip endpoint.
data MP4

instance Accept MP4 where
  contentType _ = "video" M.// "mp4"

instance MimeRender MP4 ByteString where
  mimeRender _ = LBS.fromStrict

-- | A response carrying a long-lived immutable cache header, for content-addressed
-- media (an event snapshot/clip or a proof frame never changes for a given id).
type Cached a = Headers '[Header "Cache-Control" Text] a

cached :: a -> Cached a
cached = addHeader "public, max-age=86400, immutable"
