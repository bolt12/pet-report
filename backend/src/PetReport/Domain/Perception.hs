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
  , applyCorrectionAt
  , addSighting
  , removeSightingAt
  , hasSightingAt
  , SceneEdit (..)
  , applyEditAt
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
import           PetReport.Domain.Behavior (Behaviors (..), noBehaviors,
                                            normalizeBehaviors)
import           PetReport.Domain.Types    (Activity (..), Confidence, PetId,
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
        -- Optional so the blobs written before it was asked for still decode. The
        -- description no longer offers null as a choice, since the vision prompt requires a
        -- value; the schema itself cannot demand one without failing on those old rows.
        <*> optionalFieldOrNull "confidence" "0.0 to 1.0, always present" .= confidence

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

-- | An owner correction of one mis-identified subject. @ToSpecies@ and @ToPerson@ re-target
-- that sighting inside the scene blob. @ToPet@ and @ToVisiting@ are /individual/
-- attributions (this exact cat, or a visiting animal that is not mine), stored as overrides
-- at the DB layer and never written into the model-facing scene blob.
--
-- A correction always names the sighting it applies to; see 'applyCorrectionAt'. A moment
-- holding two cats and a sitter is three sightings, and each is corrected on its own.
data Correction
  = ToSpecies Species
  | ToPerson
  | -- | Name an individual. Carries the pet's species as well as its id, because naming
    -- an individual also settles what KIND of thing was seen: if the model called it a dog
    -- and the owner says it is Mochi the cat, then the species was wrong too. Without the
    -- species here, the stored reading kept saying dog while the identity said Mochi, so a
    -- species filter missed a moment a pet filter returned, and naming a mis-read person
    -- was refused outright.
    ToPet PetId Species
  | ToVisiting
  deriving stock (Eq, Show)

-- | Retarget the appearance at @ix@, leaving every other sighting in the scene alone.
-- Out-of-range indices and sounds are left unchanged, so the function is total.
--
-- Unlike the scene-wide version this replaced, a correction can turn a person into an
-- animal and back: the owner picked that exact sighting, so their intent is unambiguous.
--
-- @ToPet@ and @ToVisiting@ do NOT touch the blob, which stays species-level so the
-- @raw_perception@ audit trail and the model schema both survive. Their effect lives in the
-- @subject_identity@ table and is applied at read time by 'identifyWith'.
applyCorrectionAt :: Int -> Correction -> Perception -> Perception
applyCorrectionAt ix corr (Seen sc) =
  Seen sc {appearances = zipWith retargetOne [0 ..] (appearances sc)}
  where
    retargetOne i ap
      | i /= ix = ap
      | otherwise = case corr of
          ToSpecies sp -> ap {who = AnAnimal sp}
          ToPerson     -> ap {who = APerson}
          -- Settle the species too, so the reading agrees with the name.
          ToPet _ sp   -> ap {who = AnAnimal sp}
          ToVisiting   -> ap
applyCorrectionAt _ _ p = p

-- | Append a sighting the model did not report, as a neutral appearance of @w@.
--
-- The model misses subjects: a person half out of frame, a second cat behind the first. Up
-- to now the owner could only RETARGET what was reported, so a frame the model read as one
-- cat could be called a cat or a person and never both. That is the "exclusively one or the
-- other" the review flow was stuck in.
--
-- Appending rather than inserting is what keeps the existing sighting indices valid, so
-- every identity override already written still points at the subject it was written for.
-- The activity is 'Unclear' and the behaviours empty because the owner is asserting
-- PRESENCE, not what the subject was doing; they can edit that afterwards like any other
-- sighting. A sound has no sightings to add to.
addSighting :: Who -> Perception -> Perception
addSighting w (Seen sc) =
  Seen sc {appearances = appearances sc ++ [Appearance w Unclear noBehaviors Nothing]}
addSighting _ p = p

-- | Drop the sighting at @ix@, for a subject the model invented.
--
-- This is the one operation that RENUMBERS: every later sighting shifts down one, so the
-- caller must remap the identity overrides in the same transaction or they will point at
-- the wrong subjects. 'PetReport.Effect.Db.Queries.removeSighting' is that caller.
removeSightingAt :: Int -> Perception -> Perception
removeSightingAt ix (Seen sc) =
  Seen sc {appearances = [ap | (i, ap) <- zip [0 ..] (appearances sc), i /= ix]}
removeSightingAt _ p = p

-- | Whether a scene actually holds a sighting at this index. The correction and edit write
-- paths check this before touching anything, so addressing a sighting that is not there is
-- a 404 rather than a silent no-op.
hasSightingAt :: Int -> Perception -> Bool
hasSightingAt ix (Seen sc) = ix >= 0 && ix < length (appearances sc)
hasSightingAt _ _          = False

-- | A flat, all-optional owner edit of one sighting. Each set field overwrites; an unset
-- field is left unchanged. Activity, location and behaviour flags apply to the addressed
-- sighting alone. Description and wellbeing are scene-level and apply whatever is addressed.
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

-- | Apply an edit, with the per-appearance fields landing on sighting @ix@ only. The
-- scene-level description and wellbeing apply regardless, so an owner can correct the
-- note on a moment whose sightings they are not touching.
applyEditAt :: Int -> SceneEdit -> Perception -> Perception
applyEditAt ix e (Seen sc) =
  Seen
    sc
      { description = maybe (description sc) blankToNothing (seDescription e)
      , wellbeing = fromMaybe (wellbeing sc) (seWellbeing e)
      , appearances = zipWith editOne [0 ..] (appearances sc)
      }
  where
    editOne i ap = if i == ix then editAppearance e ap else ap
applyEditAt _ _ p = p

-- | Overwrite the set fields of one appearance. A person sighting keeps its behaviour
-- record: with per-sighting addressing the owner is editing exactly the subject they
-- picked, so there is no longer a reason to guess that they meant an animal.
editAppearance :: SceneEdit -> Appearance -> Appearance
editAppearance e ap =
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
