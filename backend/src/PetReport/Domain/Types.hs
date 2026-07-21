-- | Core scalar domain types: the closed enumerations the code counts and
-- branches on, and the newtype-wrapped identifiers and scalars. Open-world
-- vocabularies (species) are newtypes over 'Text' so a new value needs no code
-- change; the known set is steered via the prompt, not the type.
module PetReport.Domain.Types
  ( -- * Enumerations (closed core)
    Activity (..)
  , activityText
  , Wellbeing (..)
  , wellbeingText

    -- * Open vocabularies
  , Species (..)
  , speciesText

    -- * Identifiers
  , PetId (..)
  , petIdText
  , EventId (..)
  , ObsId (..)
  , Camera (..)
  , cameraText

    -- * Infrastructure scalars
  , BaseUrl (..)
  , baseUrlText
  , ModelName (..)
  , modelNameText

    -- * Scalars
  , Confidence
  , mkConfidence
  , confidenceValue

    -- * JSON helpers
  , enumOptions
  ) where

import           Autodocodec     (Autodocodec (..), HasCodec (..),
                                  boundedEnumCodec, dimapCodec)
import           Data.Aeson      (FromJSON, Options (..), ToJSON, camelTo2,
                                  defaultOptions)
import           Data.Int        (Int64)
import           Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import           Data.Text       (Text)

-- | The dominant posture or action of one subject. 'Eliminating' makes toileting
-- a first-class activity; the normal-versus-accident detail lives in the behaviour.
data Activity
  = Sleeping
  | Resting
  | Sitting
  | Standing
  | Walking
  | Running
  | Jumping
  | Playing
  | Eating
  | Drinking
  | Grooming
  | Eliminating
  | Alert
  | Absent
  | Unclear
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec Activity)

instance HasCodec Activity where
  codec = boundedEnumCodec activityText

-- | The wire and display text for an 'Activity' (e.g. 'Sleeping' becomes @sleeping@).
activityText :: Activity -> Text
activityText a = case a of
  Sleeping    -> "sleeping"
  Resting     -> "resting"
  Sitting     -> "sitting"
  Standing    -> "standing"
  Walking     -> "walking"
  Running     -> "running"
  Jumping     -> "jumping"
  Playing     -> "playing"
  Eating      -> "eating"
  Drinking    -> "drinking"
  Grooming    -> "grooming"
  Eliminating -> "eliminating"
  Alert       -> "alert"
  Absent      -> "absent"
  Unclear     -> "unclear"

-- | Overall gestalt of a subject's wellbeing.
data Wellbeing
  = Normal
  | Concerning
  | Unsure
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec Wellbeing)

instance HasCodec Wellbeing where
  codec = boundedEnumCodec wellbeingText

-- | Wire text for a 'Wellbeing'. 'Unsure' serialises as @unclear@: the wire word
-- matches the model vocabulary, while the constructor name avoids clashing with the
-- 'Unclear' of 'Activity'.
wellbeingText :: Wellbeing -> Text
wellbeingText w = case w of
  Normal     -> "normal"
  Concerning -> "concerning"
  Unsure     -> "unclear"

-- | An animal species. Open by design (cat, dog, bird, snake, pig, ...): the
-- known set lives in the user's profile and steers the prompt.
newtype Species = Species Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, HasCodec)

speciesText :: Species -> Text
speciesText (Species s) = s

-- | An owner-assigned pet identifier, stable across renames and unique within the roster.
newtype PetId = PetId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

petIdText :: PetId -> Text
petIdText (PetId t) = t

-- | A Frigate event id, as issued by Frigate. Stored per observation and checked at
-- ingest so the same event is never processed twice.
newtype EventId = EventId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | An observation's identifier: the SQLite rowid of its @observations@ row,
-- assigned in insert order. A larger 'ObsId' is therefore a more recently stored
-- moment, which 'PetReport.Analysis.Agent.recentFirst' relies on to order surfaced
-- ids by recency without re-reading timestamps.
newtype ObsId = ObsId Int64
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | A camera identifier (a Frigate camera name), taken as-is from Frigate payloads.
-- Path safety for proof images is enforced at the media boundary by
-- 'PetReport.Web.Media.validCam', not by construction.
newtype Camera = Camera Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

cameraText :: Camera -> Text
cameraText (Camera t) = t

-- | The base URL of an HTTP service the app calls out to (Frigate and the model
-- server), e.g. @http:\/\/localhost:8080@. A newtype so a URL and a model name
-- can never be passed in the wrong order.
newtype BaseUrl = BaseUrl Text
  deriving stock (Eq, Ord, Show)

baseUrlText :: BaseUrl -> Text
baseUrlText (BaseUrl t) = t

-- | The vision model's name as the OpenAI-compatible server knows it, sent as the
-- @model@ field of each request.
newtype ModelName = ModelName Text
  deriving stock (Eq, Ord, Show)

modelNameText :: ModelName -> Text
modelNameText (ModelName t) = t

-- | A model confidence, clamped to @[0, 1]@ by 'mkConfidence'.
newtype Confidence = Confidence Double
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Confidence)

instance HasCodec Confidence where
  codec =
    dimapCodec
      (mkConfidence . toRealFloat)
      (fromFloatDigits . confidenceValue)
      (codec @Scientific)

-- | Build a 'Confidence', clamping the input to @[0, 1]@.
mkConfidence :: Double -> Confidence
mkConfidence = Confidence . min 1 . max 0

confidenceValue :: Confidence -> Double
confidenceValue (Confidence d) = d

-- | Aeson options for nullary-sum "enum" types: snake-cased constructor names
-- (@HealthConcerns@ becomes @health_concerns@).
enumOptions :: Options
enumOptions = defaultOptions {constructorTagModifier = camelTo2 '_'}
