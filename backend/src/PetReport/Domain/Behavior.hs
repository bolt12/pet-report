-- | Tracked behavioural facts for a single subject. A flat record, so each fact is
-- present-or-absent by construction and the JSON schema the model sees stays simple.
-- Elimination is structured so normal toileting is never conflated with an accident, and
-- health concerns are an open list.
module PetReport.Domain.Behavior
  ( Behaviors (..)
  , noBehaviors
  , normalizeBehaviors
  , accidentSuspected
  , injurySuspected
  , Elimination (..)
  , EliminationType (..)
  , EliminationPlace (..)
  , HealthSignal (..)
  ) where

import           Autodocodec            (Autodocodec (..), HasCodec (..),
                                         bimapCodec, boundedEnumCodec, object,
                                         optionalFieldOrNull, requiredField,
                                         (.=))
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Text              (Text)
import           PetReport.Domain.Types (Activity (..))

data EliminationType
  = Urine
  | Feces
  | UnknownElim
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec EliminationType)

instance HasCodec EliminationType where
  codec = boundedEnumCodec $ \case
    Urine -> "urine"
    Feces -> "feces"
    UnknownElim -> "unknown"

-- | Where the elimination happened. 'Inappropriate' is what counts as an accident.
data EliminationPlace
  = LitterBox
  | Outdoors
  | Inappropriate
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec EliminationPlace)

instance HasCodec EliminationPlace where
  codec = boundedEnumCodec $ \case
    LitterBox -> "litter_box"
    Outdoors -> "outdoors"
    Inappropriate -> "inappropriate"

data Elimination = Elimination
  { elimType  :: EliminationType
  , elimPlace :: EliminationPlace
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Elimination)

instance HasCodec Elimination where
  codec =
    object "Elimination" $
      Elimination
        <$> requiredField "type" "urine / feces / unknown" .= elimType
        <*> requiredField "place" "litter_box / outdoors / inappropriate" .= elimPlace

-- | A specific health concern. Closed core plus an open escape for the long tail.
data HealthSignal
  = PossibleInjury
  | Limping
  | Lethargy
  | OtherConcern Text
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec HealthSignal)

instance HasCodec HealthSignal where
  codec = bimapCodec (Right . parse) render (codec @Text)
    where
      parse t = case t of
        "possible_injury" -> PossibleInjury
        "limping"         -> Limping
        "lethargy"        -> Lethargy
        other             -> OtherConcern other
      render s = case s of
        PossibleInjury -> "possible_injury"
        Limping        -> "limping"
        Lethargy       -> "lethargy"
        OtherConcern t -> t

-- | The tracked facts observed for one subject in one frame or clip. Every field is
-- strict. Behaviours sit inside every 'Appearance' of every bulk-decoded scene, so a lazy
-- field would leave one thunk per subject per row. The 'Maybe' and list fields force to
-- their constructor and spine, so an absent elimination stays absent.
data Behaviors = Behaviors
  { ate        :: !Bool
  , drank      :: !Bool
  , slept      :: !Bool
  , played     :: !Bool
  , groomed    :: !Bool
  , eliminated :: !(Maybe Elimination)
  , concerns   :: ![HealthSignal]
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Behaviors)

instance HasCodec Behaviors where
  codec =
    object "Behaviors" $
      Behaviors
        <$> requiredField "ate" "true only if visibly consuming food, not merely near a bowl" .= ate
        <*> requiredField "drank" "true only if visibly drinking water, not merely near a bowl" .= drank
        <*> requiredField "slept" "sleeping or napping" .= slept
        <*> requiredField "played" "clearly playing, not just moving" .= played
        <*> requiredField "groomed" "washing or being brushed" .= groomed
        <*> optionalFieldOrNull "eliminated" "toileting details, or null; only when clearly visible (a crouching animal alone is not enough)" .= eliminated
        <*> requiredField "concerns" "visible health concerns only (limp, injury, lethargy); [] if none" .= concerns

noBehaviors :: Behaviors
noBehaviors =
  Behaviors
    { ate = False
    , drank = False
    , slept = False
    , played = False
    , groomed = False
    , eliminated = Nothing
    , concerns = []
    }

-- | Enforce activity/behaviour consistency by construction: an eating activity implies
-- @ate@, and so on. Cheaper and more reliable than asking the model to stay consistent.
normalizeBehaviors :: Activity -> Behaviors -> Behaviors
normalizeBehaviors act b =
  b
    { ate = ate b || act == Eating
    , drank = drank b || act == Drinking
    , slept = slept b || act == Sleeping
    , played = played b || act == Playing
    , groomed = groomed b || act == Grooming
    }

-- | An accident is an elimination in an inappropriate place.
accidentSuspected :: Behaviors -> Bool
accidentSuspected b = case eliminated b of
  Just e  -> elimPlace e == Inappropriate
  Nothing -> False

-- | An injury is suspected when a pain/limp health signal is present.
injurySuspected :: Behaviors -> Bool
injurySuspected b = any isInjury (concerns b)
  where
    isInjury s = case s of
      PossibleInjury -> True
      Limping        -> True
      _              -> False
