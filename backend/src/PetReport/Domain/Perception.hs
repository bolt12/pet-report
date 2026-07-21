-- | What a camera perceived, split by modality: a visual 'Scene' (a set of
-- per-subject appearances) or a 'Heard' sound. Only 'Scene' and its components
-- are model-facing (schema-constrained); 'Perception' itself is a storage/API type.
module PetReport.Domain.Perception
  ( Perception (..)
  , SoundKind (..)
  , soundPhrase
  , isSafetySound
  , Scene (..)
  , emptyScene
  , normalizeScene
  , Appearance (..)
  , Who (..)
  , isPerson
  , animalSpecies
  , sceneSchema
  , Correction (..)
  , applyCorrection
  , SceneEdit (..)
  , applyEdit
  ) where

import           Autodocodec               (Autodocodec (..), HasCodec (..),
                                            bimapCodec, object,
                                            optionalFieldOrNull, requiredField,
                                            (.=))
import           Data.Aeson                (FromJSON (..), ToJSON (..), Value)
import qualified Data.Aeson                as A
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           PetReport.Domain.Behavior (Behaviors (..), normalizeBehaviors)
import           PetReport.Domain.Types    (Activity, Confidence, PetId,
                                            Species (..), Wellbeing (..))
import           LLM.Schema                (codecSchema)

-- | Who is present, at the level a camera can perceive: a species, or a person.
-- No identity claim (which pet) is made here; that is derived against the roster.
data Who
  = AnAnimal Species
  | APerson
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Who)

instance HasCodec Who where
  codec = bimapCodec (Right . parse) render (codec @Text)
    where
      parse t = if t == "person" then APerson else AnAnimal (Species t)
      render w = case w of
        APerson              -> "person"
        AnAnimal (Species t) -> t

-- | Whether a perceived subject is a person rather than an animal.
isPerson :: Who -> Bool
isPerson APerson      = True
isPerson (AnAnimal _) = False

-- | The species of an animal subject, or 'Nothing' for a person.
animalSpecies :: Who -> Maybe Species
animalSpecies (AnAnimal s) = Just s
animalSpecies APerson      = Nothing

-- | One visible subject and what it is doing. Strict throughout, since appearances ride
-- inside a 'Scene' decoded in bulk and a lazy field would leave a per-row thunk in the
-- retained list. The 'Maybe' forces to its constructor, not its value.
data Appearance = Appearance
  { who       :: !Who
  , activity  :: !Activity
  , behaviors :: !Behaviors
  , whereAt   :: !(Maybe Text)
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Appearance)

instance HasCodec Appearance where
  codec =
    object "Appearance" $
      Appearance
        <$> requiredField "who" "cat / dog / person / other species" .= who
        <*> requiredField "activity" "the dominant posture or action" .= activity
        <*> requiredField "behaviors" "tracked behaviours for this subject" .= behaviors
        <*> optionalFieldOrNull "where" "location in the room, or null" .= whereAt

-- | A visual scene: zero or more per-subject appearances plus scene-level notes.
-- Strict for the same reason as 'Appearance'; the appearance list forces to its spine.
data Scene = Scene
  { appearances :: ![Appearance]
  , notable     :: !(Maybe Text)
  , description :: !(Maybe Text)
  , wellbeing   :: !Wellbeing
  , confidence  :: !(Maybe Confidence)
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Scene)

instance HasCodec Scene where
  codec =
    object "Scene" $
      Scene
        <$> requiredField "appearances" "one object per visible animal or person" .= appearances
        <*> optionalFieldOrNull "notable" "anything genuinely noteworthy, or null" .= notable
        <*> optionalFieldOrNull "description" "one neutral sentence, or null" .= description
        <*> requiredField "wellbeing" "normal / concerning / unclear" .= wellbeing
        <*> optionalFieldOrNull "confidence" "0.0 to 1.0, or null" .= confidence

emptyScene :: Scene
emptyScene =
  Scene
    { appearances = []
    , notable = Nothing
    , description = Nothing
    , wellbeing = Normal
    , confidence = Nothing
    }

-- | Enforce activity/behaviour consistency on every appearance, repairing whatever the
-- model returned. Applied at the model-response decode in "PetReport.Analysis.Vision",
-- before storage. The 'Scene' codec itself does not normalize.
normalizeScene :: Scene -> Scene
normalizeScene sc = sc {appearances = map norm (appearances sc)}
  where
    norm ap = ap {behaviors = normalizeBehaviors (activity ap) (behaviors ap)}

-- | A microphone detection, holding the raw Frigate audio label (e.g.
-- @smoke_detector@). Display and safety classification are derived from it.
newtype SoundKind = SoundKind Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | A friendly phrase for a raw audio label, e.g. @smoke_detector@ -> "a smoke
-- alarm". Unknown labels fall back to the label with underscores spaced out.
soundPhrase :: SoundKind -> Text
soundPhrase (SoundKind l) = case l of
  "bark"           -> "a bark"
  "bow-wow"        -> "barking"
  "howl"           -> "a howl"
  "growling"       -> "growling"
  "whimper_dog"    -> "a whimper"
  "meow"           -> "a meow"
  "speech"         -> "talking"
  "yell"           -> "a shout"
  "doorbell"       -> "the doorbell"
  "ding-dong"      -> "the doorbell"
  "knock"          -> "a knock at the door"
  "fire_alarm"     -> "a fire alarm"
  "smoke_detector" -> "a smoke alarm"
  "smoke_alarm"    -> "a smoke alarm"
  "co_alarm"       -> "a carbon monoxide alarm"
  "siren"          -> "a siren"
  "car_alarm"      -> "a car alarm"
  "glass"          -> "breaking glass"
  "shatter"        -> "shattering"
  "breaking"       -> "something breaking"
  "thump"          -> "a thump"
  "bang"           -> "a bang"
  _                -> T.map (\c -> if c == '_' then ' ' else c) l

-- | Safety-relevant sounds that should raise the daily report's alert priority.
isSafetySound :: SoundKind -> Bool
isSafetySound (SoundKind l) =
  l `elem` ["fire_alarm", "smoke_detector", "smoke_alarm", "co_alarm", "siren", "car_alarm", "glass", "shatter", "breaking"]

data Perception
  = Seen Scene
  | Heard SoundKind
  deriving stock (Eq, Show)

instance ToJSON Perception where
  toJSON p = case p of
    Seen s  -> A.object ["kind" A..= ("scene" :: Text), "scene" A..= s]
    Heard k -> A.object ["kind" A..= ("sound" :: Text), "sound" A..= k]

instance FromJSON Perception where
  parseJSON = A.withObject "Perception" $ \o -> do
    k <- o A..: "kind"
    case (k :: Text) of
      "scene" -> Seen <$> o A..: "scene"
      "sound" -> Heard <$> o A..: "sound"
      other   -> fail ("unknown perception kind: " <> show other)

-- | The JSON Schema the vision model is constrained to via @response_format@.
-- Derived from the 'Scene' codec, so it always describes what 'Scene' will parse.
sceneSchema :: Value
sceneSchema = codecSchema @Scene

-- | An owner correction of a mis-identified subject. @ToSpecies@ and @ToPerson@ re-target
-- the animal appearances of a scene, leaving people alone. @ToPet@ and @ToVisiting@ are
-- /individual/ attributions (this exact cat, or a visiting animal that is not mine), stored
-- as overrides at the DB layer and never written into the model-facing scene blob.
data Correction
  = ToSpecies Species
  | ToPerson
  | ToPet PetId
  | ToVisiting
  deriving stock (Eq, Show)

-- | Apply a correction to the stored perception. @ToSpecies@ and @ToPerson@ retarget every
-- animal appearance; a sound is unchanged.
--
-- @ToPet@ and @ToVisiting@ do NOT touch the blob, which stays species-level so the
-- @raw_perception@ audit trail and the model schema both survive. Their effect lives in the
-- @subject_identity@ table and is applied at read time by 'identifyWith'.
applyCorrection :: Correction -> Perception -> Perception
applyCorrection (ToSpecies sp) (Seen sc) = Seen sc {appearances = map (retarget (AnAnimal sp)) (appearances sc)}
applyCorrection ToPerson (Seen sc) = Seen sc {appearances = map (retarget APerson) (appearances sc)}
applyCorrection _ p = p

retarget :: Who -> Appearance -> Appearance
retarget target ap = if isPerson (who ap) then ap else ap {who = target}

-- | A flat, all-optional owner edit of a scene. Each set field overwrites; an unset field
-- is left unchanged. Activity, location and behaviour flags apply to every /animal/
-- appearance, which is exact for the common single-subject scene. Person appearances and
-- 'Heard' sounds are left alone. Description and wellbeing are scene-level.
data SceneEdit = SceneEdit
  { seActivity    :: Maybe Activity
  , seWellbeing   :: Maybe Wellbeing
  , seDescription :: Maybe Text
  , seWhereAt     :: Maybe Text
  , seAte         :: Maybe Bool
  , seDrank       :: Maybe Bool
  , seSlept       :: Maybe Bool
  , sePlayed      :: Maybe Bool
  , seGroomed     :: Maybe Bool
  }
  deriving stock (Eq, Show)

applyEdit :: SceneEdit -> Perception -> Perception
applyEdit e (Seen sc) =
  Seen
    sc
      { description = maybe (description sc) blankToNothing (seDescription e)
      , wellbeing = fromMaybe (wellbeing sc) (seWellbeing e)
      , appearances = map (editAppearance e) (appearances sc)
      }
applyEdit _ p = p

editAppearance :: SceneEdit -> Appearance -> Appearance
editAppearance e ap
  | isPerson (who ap) = ap
  | otherwise =
      ap
        { activity = fromMaybe (activity ap) (seActivity e)
        , whereAt = maybe (whereAt ap) blankToNothing (seWhereAt e)
        , behaviors = editBehaviors e (behaviors ap)
        }

editBehaviors :: SceneEdit -> Behaviors -> Behaviors
editBehaviors e b =
  b
    { ate = fromMaybe (ate b) (seAte e)
    , drank = fromMaybe (drank b) (seDrank e)
    , slept = fromMaybe (slept b) (seSlept e)
    , played = fromMaybe (played b) (sePlayed e)
    , groomed = fromMaybe (groomed b) (seGroomed e)
    }

-- | Treat an all-whitespace edit value as "clear this optional field".
blankToNothing :: Text -> Maybe Text
blankToNothing t = if T.null (T.strip t) then Nothing else Just t
