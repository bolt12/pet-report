-- | The presentation view of an observation: everything the SPA needs to render a timeline
-- card. Identity, chips, uncertainty and the room label are all /derived/ here, so the
-- client never re-implements domain semantics.
--
-- Media needs config and the clock, so the web layer supplies it through 'ObsMedia'.
-- Everything else is pure and testable.
module PetReport.Domain.View
  ( ObsView (..)
  , ObsMedia (..)
  , SubjectRef (..)
  , Chip (..)
  , ChipKind (..)
  , viewOf
  , mkViews
  , mediaFor
  , RetentionMap
  , momentExpiry
  , subjectLabelOf
  , chipsOf
  , fallbackDescription
  ) where

import           Data.Aeson            (ToJSON (..), Value (String),
                                        genericToJSON, object, (.=))
import           Data.Function         (on)
import           Data.Int              (Int64)
import           Data.List             (nub, nubBy)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (listToMaybe)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (UTCTime, addUTCTime,
                                        diffUTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           GHC.Generics          (Generic)

import qualified PetReport.Domain.Behavior    as B
import           PetReport.Domain.Observation (FrigateMeta (..),
                                               Observation (..), Origin (..))
import qualified PetReport.Domain.Observation as O
import qualified PetReport.Domain.Perception  as P
import           PetReport.Domain.Profile     (Overrides, Profile (..), roomOf)
import           PetReport.Domain.Stats       (identifiedAppearances,
                                               isUncertain)
import qualified PetReport.Domain.Profile     as Pr
import           PetReport.Domain.Types       (Camera, EventId (..), ObsId (..),
                                               activityText,
                                               cameraText, confidenceValue,
                                               petIdText, speciesText,
                                               wellbeingText)
import           PetReport.Util               (capitalize, prefixed)

-- | A resolved subject on a card.
data SubjectRef = SubjectRef
  { srPetId   :: Maybe Text
  , srLabel   :: Text
  , srSpecies :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SubjectRef where
  toJSON = genericToJSON (prefixed 2)

data ChipKind = Good | Watch | Info
  deriving stock (Eq, Show)

instance ToJSON ChipKind where
  toJSON k =
    String $ case k of
      Good  -> "good"
      Watch -> "watch"
      Info  -> "info"

-- | A small status pill (a tracked fact) on a card.
data Chip = Chip
  { chLabel :: Text
  , chKind  :: ChipKind
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Chip where
  toJSON = genericToJSON (prefixed 2)

-- | Resolved media for one observation, filled by the web layer.
data ObsMedia = ObsMedia
  { omKind :: Text
  -- ^ "photo" | "clip" | "audio" | "expired".
  , omImg  :: Maybe Text
  , omClip :: Maybe Text
  }
  deriving stock (Eq, Show)

data ObsView = ObsView
  { ovId           :: Int64
  , ovAt           :: UTCTime
  , ovCamera       :: Text
  , ovRoom         :: Text
  , ovSubjectLabel :: Text
  , ovSubjects     :: [SubjectRef]
  , ovActivity     :: Maybe Text
  , ovWhereAt      :: Maybe Text
  , ovChips        :: [Chip]
  , ovWellbeing    :: Text
  -- ^ "normal" | "concerning" | "unclear" | "none" (audio).
  , ovConfidence   :: Maybe Double
  , ovUncertain    :: Bool
  , ovDescription  :: Maybe Text
  , ovMedia        :: Text
  , ovImg          :: Maybe Text
  , ovClip         :: Maybe Text
  , ovTranscript   :: Maybe Text
  , ovNeedsReview  :: Bool
  , ovReviewed     :: Bool
  , ovKept         :: Bool
  -- ^ Whether the owner kept this moment. Kept media belongs to pet-report, so it never
  -- expires and shows no countdown.
  , ovClipExpiresAt :: Maybe UTCTime
  -- ^ When Frigate is expected to prune this moment's borrowed footage, for the UI's
  -- "keep it before it's gone" countdown. 'Nothing' for a kept moment, a locally-owned
  -- sample, or an unknown retention. 'mkViews' fills it from the polled retention map;
  -- 'viewOf' leaves it 'Nothing' to stay pure.
  }
  deriving stock (Generic)

-- Emits the contract Moment read model. The flat media/img/clip fields group into a nested
-- @media { kind, stillUrl?, clipUrl? }@ object and @ovWhereAt@ surfaces as @location@;
-- every other field keeps its 'prefixed 2' name.
instance ToJSON ObsView where
  toJSON v =
    object
      [ "id" .= ovId v
      , "at" .= ovAt v
      , "camera" .= ovCamera v
      , "room" .= ovRoom v
      , "subjectLabel" .= ovSubjectLabel v
      , "subjects" .= ovSubjects v
      , "activity" .= ovActivity v
      , "location" .= ovWhereAt v
      , "chips" .= ovChips v
      , "wellbeing" .= ovWellbeing v
      , "confidence" .= ovConfidence v
      , "uncertain" .= ovUncertain v
      , "description" .= ovDescription v
      , "media" .= object ["kind" .= ovMedia v, "stillUrl" .= ovImg v, "clipUrl" .= ovClip v]
      , "transcript" .= ovTranscript v
      , "reviewed" .= ovReviewed v
      , "needsReview" .= ovNeedsReview v
      , "kept" .= ovKept v
      , "clipExpiresAt" .= ovClipExpiresAt v
      ]

-- | Build the view for one observation. @roomFor@ maps a camera to its label;
-- @mediaOf@ resolves media (photo/clip/audio/expired + urls).
viewOf ::
  Pr.Roster ->
  Pr.Overrides ->
  (Camera -> Text) ->
  (O.Observation -> ObsMedia) ->
  O.Observation ->
  ObsView
viewOf roster ov roomFor mediaOf obs =
  let ObsId oid = O.obsId obs
      m = mediaOf obs
      cam = O.camera obs
      card lbl subs act loc' chps well conf unc desc =
        ObsView
          { ovId = oid
          , ovAt = O.at obs
          , ovCamera = cameraText cam
          , ovRoom = roomFor cam
          , ovSubjectLabel = lbl
          , ovSubjects = subs
          , ovActivity = act
          , ovWhereAt = loc'
          , ovChips = chps
          , ovWellbeing = well
          , ovConfidence = conf
          , ovUncertain = unc
          , ovDescription = desc
          , ovMedia = omKind m
          , ovImg = omImg m
          , ovClip = omClip m
          -- Filled by the web layer from the DB cache (mkViews); the domain view
          -- itself has no transcript, keeping viewOf pure and testable.
          , ovTranscript = Nothing
          , ovNeedsReview = unc && not (O.reviewed obs)
          , ovReviewed = O.reviewed obs
          -- Kept-ness and clip expiry are DB/config concerns the pure view does not
          -- carry; 'mkViews' patches them from the kept-set and the retention map.
          , ovKept = False
          , ovClipExpiresAt = Nothing
          }
   in case O.perception obs of
        P.Seen sc ->
          let aps = P.appearances sc
              idents = map fst (identifiedAppearances ov roster obs)
              subs = map subjectRefOf idents
              lbl = subjectLabelOf subs
              baseChips = chipsOf sc
              -- A reassigned-to-visitor animal reads as a visitor even with no
              -- person in frame; add the pill unless the person path already did.
              chps =
                if any isVisitingAnimal idents && not (any ((== "visitor") . chLabel) baseChips)
                  then baseChips ++ [Chip "visitor" Info]
                  else baseChips
              -- Attribute the card's activity/location to the primary animal, so a
              -- "Visitor + Miso" card is not tagged with the visitor's action;
              -- fall back to the first appearance for a person-only scene.
              primaryAp = case filter (not . isPersonAp) aps of
                (a : _) -> Just a
                []      -> listToMaybe aps
              act = activityText . P.activity <$> primaryAp
              loc = primaryAp >>= P.whereAt
              conf = confidenceValue <$> P.confidence sc
              unc = isUncertain (O.perception obs)
              desc = case P.description sc of
                Just d | not (T.null (T.strip d)) -> d
                _ -> fallbackDescription lbl act loc (roomFor cam)
           in card
                lbl
                subs
                act
                loc
                chps
                (wellbeingText (P.wellbeing sc))
                conf
                unc
                (Just desc)
        P.Heard sk ->
          let safety = P.isSafetySound sk
           in card
                "A sound"
                []
                Nothing
                Nothing
                [Chip (if safety then "safety" else "sound") (if safety then Watch else Info)]
                (if safety then "concerning" else "none")
                Nothing
                False
                (Just ("Heard " <> P.soundPhrase sk))

subjectRefOf :: Pr.Identity -> SubjectRef
subjectRefOf idn = case idn of
  Pr.KnownPet p ->
    SubjectRef (Just (petIdText (Pr.petId p))) (Pr.petName p) (Just (speciesText (Pr.petSpecies p)))
  Pr.UnknownAnimal sp ->
    SubjectRef Nothing (capitalize (speciesText sp)) (Just (speciesText sp))
  Pr.Visiting sp ->
    SubjectRef Nothing ("A visiting " <> speciesText sp) (Just (speciesText sp))
  Pr.Human ->
    SubjectRef Nothing "Visitor" Nothing

isVisitingAnimal :: Pr.Identity -> Bool
isVisitingAnimal (Pr.Visiting _) = True
isVisitingAnimal _               = False

isPersonAp :: P.Appearance -> Bool
isPersonAp = P.isPerson . P.who

-- | Join subject labels, e.g. "Visitor + Miso"; empty subjects read "Unclear".
subjectLabelOf :: [SubjectRef] -> Text
subjectLabelOf []  = "Unclear"
subjectLabelOf srs = T.intercalate " + " (nub (map srLabel srs))

-- | A neutral one-line description synthesised from the structured scene, for when the
-- model omitted its own and the card still needs a readable line. Prefers the specific
-- location, falling back to the room.
--
-- >>> fallbackDescription "Miso" (Just "resting") (Just "on the sofa") "Living Room"
-- "Miso resting on the sofa."
-- >>> fallbackDescription "Miso" (Just "sleeping") Nothing "Living Room"
-- "Miso sleeping in the Living Room."
fallbackDescription :: Text -> Maybe Text -> Maybe Text -> Text -> Text
fallbackDescription subject mAct mWhere room =
  subject <> verb <> place <> "."
  where
    verb = case mAct of
      Just a | not (T.null a) && a /= "unclear" && a /= "absent" -> " " <> a
      _                                                          -> ""
    place = case mWhere of
      Just w | not (T.null (T.strip w)) -> " " <> T.strip w
      _                                 -> " in the " <> room

-- | The status pills for a scene: behaviour facts, then a visitor pill, then a
-- notable pill; deduplicated by label, order preserved.
chipsOf :: P.Scene -> [Chip]
chipsOf sc =
  nubBy ((==) `on` chLabel) $
    concatMap apChips (P.appearances sc)
      ++ [Chip "visitor" Info | any isPersonAp (P.appearances sc)]
      ++ [Chip "notable" Info | notablePresent]
  where
    notablePresent = case P.notable sc of
      Just _  -> True
      Nothing -> False

apChips :: P.Appearance -> [Chip]
apChips ap =
  let b = P.behaviors ap
   in concat
        [ [Chip "ate" Good | B.ate b]
        , [Chip "drank" Good | B.drank b]
        , [Chip "slept" Good | B.slept b]
        , [Chip "played" Good | B.played b]
        , [Chip "groomed" Good | B.groomed b]
        , elimChips (B.eliminated b)
        , concatMap concernChip (B.concerns b)
        ]

elimChips :: Maybe B.Elimination -> [Chip]
elimChips Nothing = []
elimChips (Just e) = case B.elimPlace e of
  B.LitterBox     -> [Chip "litter box" Good]
  B.Outdoors      -> [Chip "outdoors" Good]
  B.Inappropriate -> [Chip "accident" Watch]

concernChip :: B.HealthSignal -> [Chip]
concernChip s = case s of
  B.Limping        -> [Chip "possible limp" Watch]
  B.PossibleInjury -> [Chip "possible injury" Watch]
  B.Lethargy       -> [Chip "lethargy" Watch]
  B.OtherConcern t -> [Chip t Watch]

-- --------------------------------------------------------------------------- --
-- View + media
-- --------------------------------------------------------------------------- --

-- | Per-camera Frigate media retention, in days. Polled into 'PetReport.App.appRetention'
-- and read here on demand, so a config change needs nothing stored or propagated.
type RetentionMap = Map Text Double

-- | Build the day's views, then splice in the DB and config concerns the pure 'viewOf' does
-- not carry: cached transcripts, whether each moment is kept, and its clip-expiry.
-- @retainDays@ is the proof-retention window threaded to 'mediaFor', @retention@ the polled
-- Frigate map, @kept@ the set of kept observation ids.
mkViews ::
  Double ->
  RetentionMap ->
  Set ObsId ->
  UTCTime ->
  Map ObsId Text ->
  Profile ->
  Overrides ->
  [Observation] ->
  [ObsView]
mkViews retainDays retention kept now transcripts prof ov obss =
  [ patch (build obs) obs | obs <- obss ]
  where
    build = viewOf (pets prof) ov (roomOf (cameras prof)) (mediaFor retainDays now)
    patch v obs =
      let isKept = Set.member (ObsId (ovId v)) kept
       in v
            { ovTranscript = Map.lookup (ObsId (ovId v)) transcripts
            , ovKept = isKept
            , ovClipExpiresAt = if isKept then Nothing else momentExpiry retention (gcWindowDays prof) obs
            }

-- | When a moment will be gone unless kept: its start time plus the EARLIER of the GC window
-- (pet-report drops the record) and the camera's Frigate clip retention (Frigate prunes the
-- footage). Shortening the GC window therefore shows up here rather than being masked.
--
-- Only events borrow media from Frigate, so a periodic sample gets 'Nothing'. The GC window
-- always applies, so an unknown camera still yields a deadline.
momentExpiry :: RetentionMap -> Int -> Observation -> Maybe UTCTime
momentExpiry retention gcWindowDays obs = case origin obs of
  FromEvent m
    | detectorHasClip m || detectorHasSnapshot m ->
        let gcDays = fromIntegral (max 1 gcWindowDays) :: Double
            days = maybe gcDays (min gcDays) (Map.lookup (cameraText (camera obs)) retention)
         in Just (addUTCTime (realToFrac (days * 86400)) (at obs))
  _ -> Nothing

-- | Resolve an observation's media. Samples serve a proof frame; events serve their Frigate
-- snapshot and clip, re-served by the backend under @/api/events@; a sound serves its saved
-- Frigate clip as playable audio, or nothing when there is no clip. A visible moment older
-- than @retainDays@ reads as "expired". That window is passed in, keeping the domain view
-- free of the pipeline layer.
mediaFor :: Double -> UTCTime -> Observation -> ObsMedia
mediaFor retainDays now obs = case perception obs of
  -- A sound detection: serve the Frigate clip when one was saved, since its mp4 carries
  -- the recorded audio. Without one the moment stays a label with nothing to play.
  P.Heard _ -> case origin obs of
    FromEvent m | detectorHasClip m -> ObsMedia "audio" Nothing (Just (clipUrl m))
    _                               -> ObsMedia "audio" Nothing Nothing
  P.Seen _
    | ageDays > retainDays -> ObsMedia "expired" Nothing Nothing
    | otherwise -> case origin obs of
        PeriodicSample -> ObsMedia "photo" (Just proofUrl) Nothing
        FromEvent m
          -- Only advertise a clip when Frigate actually saved one; a snapshot-only event
          -- serves its snapshot as a photo instead. Likewise the poster, since a clip-only
          -- event (has_snapshot=false) has no still and its snapshot URL would 404.
          | detectorHasClip m -> ObsMedia "clip" (poster m) (Just (clipUrl m))
          | otherwise -> ObsMedia "photo" (poster m) Nothing
  where
    ageDays = realToFrac (diffUTCTime now (at obs)) / 86400 :: Double
    ts = round (utcTimeToPOSIXSeconds (at obs)) :: Integer
    cam = cameraText (camera obs)
    proofUrl = "/proof/" <> cam <> "/" <> T.pack (show ts) <> ".jpg"
    poster m = if detectorHasSnapshot m then Just (snapUrl m) else Nothing
    snapUrl m = "/api/events/" <> unEvent (eventId m) <> "/snapshot.jpg"
    clipUrl m = "/api/events/" <> unEvent (eventId m) <> "/clip.mp4"
    unEvent (EventId e) = e
