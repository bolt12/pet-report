-- | The query-param vocabulary of the moments browse: one selector type per facet, each
-- with a 'FromHttpApiData' that rejects an unrecognised token with a message naming the
-- legal set, instead of silently dropping the clause.
--
-- Two problems live here and they turn out to be the same problem. The browse used to take
-- twelve positional @Maybe Text@ parameters, any two of which could be transposed with no
-- type error; and three of those facets parsed their value while two handed raw text
-- straight to SQL. A distinct type per facet fixes both: transposition stops compiling, and
-- parsing happens at the edge, where a bad value can still be reported to whoever sent it.
--
-- The silent-drop behaviour is what let @pet=visitor@ answer 200 with an empty page. A
-- caller now hears why.
--
-- These are Web-layer selectors wrapping the persistence facet types rather than instances
-- on those types directly, which would be orphans and would drag Servant across the
-- Effect boundary.
module PetReport.Web.Facets
  ( SubjectSel (..)
  , subjectSelValues
  , resolveSubject
  , ReviewSel (..)
  , reviewValues
  , MediaSel (..)
  , mediaValues
  , TimeOfDaySel (..)
  , timeOfDayValues
  , SortSel (..)
  , sortValues
  , ActivitySel (..)
  , activityValues
  , BehaviourSel (..)
  , behaviourValues
  , WellbeingSel (..)
  , wellbeingValues
  ) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Servant                  (FromHttpApiData (..))

import           PetReport.Domain.Profile (Pet (..), Profile, petById, pets,
                                           uniquePetOfSpecies)
import           PetReport.Domain.Types   (Activity, PetId (..), Wellbeing,
                                           activityText, petIdText, speciesText,
                                           wellbeingText)
import qualified PetReport.Effect.Db      as Db

-- | The @subject@ facet as it arrives on the wire, before the roster is consulted.
--
-- @pet:mochi@, @species:cat@, @person@, @visiting@. Parsing is separate from resolving
-- because only the second needs the profile: the shape of the request is settled here, and
-- whether the named pet exists is settled in the handler, which holds the roster.
data SubjectSel
  = SelPet Text
  | SelSpecies Text
  | SelPerson
  | SelVisiting
  deriving stock (Eq, Show)

-- | The complete @subject@ vocabulary, @<...>@ standing for a caller-supplied value.
subjectSelValues :: [Text]
-- @visiting@ rather than @visitor@: a "visitor" is a human to most people, and this value
-- means an ANIMAL the owner marked as not theirs. The two were one word and neither read
-- unambiguously.
subjectSelValues = ["pet:<id>", "species:<name>", "person", "visiting"]

instance FromHttpApiData SubjectSel where
  parseQueryParam t = case T.breakOn ":" t of
    ("person", "")  -> Right SelPerson
    ("visiting", "") -> Right SelVisiting
    ("pet", rest) | Just v <- afterColon rest -> Right (SelPet v)
    ("species", rest) | Just v <- afterColon rest -> Right (SelSpecies v)
    _ -> Left (rejection "subject" t subjectSelValues)
    where
      afterColon r = let v = T.drop 1 r in if T.null v then Nothing else Just v

-- | Resolve a parsed selector against the roster. Only a pet id can fail, and it fails
-- rather than matching nothing: naming a pet the roster does not hold is a caller error,
-- and answering 200 with an empty page taught nobody anything.
--
-- A pet that is the unique ACTIVE pet of its species also claims that species'
-- unattributed sightings, the same rule the per-pet statistics use.
resolveSubject :: Profile -> SubjectSel -> Either Text Db.SubjectFilter
resolveSubject prof sel = case sel of
  SelPerson -> Right Db.SubjPerson
  SelVisiting -> Right Db.SubjVisiting
  SelSpecies sp -> Right (Db.SubjSpecies sp)
  SelPet name -> case petById roster (PetId name) of
    Nothing ->
      Left $
        "subject names a pet that is not in the roster: "
          <> name
          <> ". Known pets: "
          <> T.intercalate ", " (map (petIdText . petId) roster)
    Just p ->
      let uniq = case uniquePetOfSpecies roster (petSpecies p) of
            Just up | petId up == petId p -> Just (speciesText (petSpecies p))
            _ -> Nothing
       in Right (Db.SubjPet (Db.PetFilter (petIdText (petId p)) uniq))
  where
    roster = pets prof

-- | The @review@ facet.
newtype ReviewSel = ReviewSel Db.ReviewFilter

instance FromHttpApiData ReviewSel where
  parseQueryParam t = case t of
    "reviewed"   -> Right (ReviewSel Db.Reviewed)
    "unreviewed" -> Right (ReviewSel Db.Unreviewed)
    "needs-look" -> Right (ReviewSel Db.NeedsLook)
    _            -> Left (rejection "review" t reviewValues)

-- | The complete @review@ vocabulary.
reviewValues :: [Text]
reviewValues = ["reviewed", "unreviewed", "needs-look"]

-- | The @media@ facet.
newtype MediaSel = MediaSel Db.MediaKind

instance FromHttpApiData MediaSel where
  parseQueryParam t = case t of
    "photo" -> Right (MediaSel Db.MediaPhoto)
    "clip"  -> Right (MediaSel Db.MediaClip)
    "audio" -> Right (MediaSel Db.MediaAudio)
    _       -> Left (rejection "media" t mediaValues)

-- | The complete @media@ vocabulary.
mediaValues :: [Text]
mediaValues = ["photo", "clip", "audio"]

-- | The @timeOfDay@ facet, as a half-open local-second range. @night@ wraps midnight.
--
-- These bounds are the single definition. The client used to restate them in its own
-- bucketing helper, which is two copies of one rule in two languages.
newtype TimeOfDaySel = TimeOfDaySel Db.TimeBucket

instance FromHttpApiData TimeOfDaySel where
  parseQueryParam t = case t of
    "morning"   -> Right (TimeOfDaySel (Db.TimeBucket (5 * 3600) (12 * 3600)))
    "afternoon" -> Right (TimeOfDaySel (Db.TimeBucket (12 * 3600) (17 * 3600)))
    "evening"   -> Right (TimeOfDaySel (Db.TimeBucket (17 * 3600) (21 * 3600)))
    "night"     -> Right (TimeOfDaySel (Db.TimeBucket (21 * 3600) (5 * 3600)))
    _           -> Left (rejection "timeOfDay" t timeOfDayValues)

-- | The complete @timeOfDay@ vocabulary.
timeOfDayValues :: [Text]
timeOfDayValues = ["morning", "afternoon", "evening", "night"]

-- | The @sort@ facet.
newtype SortSel = SortSel Db.SortDir

instance FromHttpApiData SortSel where
  parseQueryParam t = case t of
    "asc"  -> Right (SortSel Db.Asc)
    "desc" -> Right (SortSel Db.Desc)
    _      -> Left (rejection "sort" t sortValues)

-- | The complete @sort@ vocabulary. @desc@ used to be everything-that-is-not-@asc@, so a
-- typo quietly ordered newest-first and looked deliberate.
sortValues :: [Text]
sortValues = ["asc", "desc"]

-- | The @activity@ facet, over the closed 'Activity' enum the model is constrained to.
newtype ActivitySel = ActivitySel Activity

-- | Every constructor round-trips through 'activityText', which is also what the facts
-- projection stores, so the facet vocabulary and the stored vocabulary are one list.
instance FromHttpApiData ActivitySel where
  parseQueryParam t = case lookup t table of
    Just a  -> Right (ActivitySel a)
    Nothing -> Left (rejection "activity" t activityValues)
    where
      table = [(activityText a, a) | a <- [minBound .. maxBound]]

-- | The complete @activity@ vocabulary, derived from the enum rather than restated.
activityValues :: [Text]
activityValues = map activityText [minBound .. maxBound]

-- | The @behaviour@ facet, over the projected behaviour columns.
newtype BehaviourSel = BehaviourSel Db.Behaviour

-- | Every per-pet tile counts one of these columns, so linking a tile to the matching
-- constructor is what makes its number and its drill-through the same set.
instance FromHttpApiData BehaviourSel where
  parseQueryParam t = case lookup t table of
    Just b  -> Right (BehaviourSel b)
    Nothing -> Left (rejection "behaviour" t behaviourValues)
    where
      table = [(behaviourWord b, b) | b <- [minBound .. maxBound]]

-- | The complete @behaviour@ vocabulary, derived from the enum rather than restated.
behaviourValues :: [Text]
behaviourValues = map behaviourWord [minBound .. maxBound]

-- | The wire word for a behaviour. Deliberately not the SQL column name: the two happen to
-- coincide today, and tying the public vocabulary to the schema would make renaming a
-- column a breaking API change.
behaviourWord :: Db.Behaviour -> Text
behaviourWord b = case b of
  Db.BehAte        -> "ate"
  Db.BehDrank      -> "drank"
  Db.BehSlept      -> "slept"
  Db.BehPlayed     -> "played"
  Db.BehGroomed    -> "groomed"
  Db.BehEliminated -> "eliminated"
  Db.BehRest       -> "rest"
  Db.BehActive     -> "active"
  Db.BehConcern    -> "concern"

-- | The @wellbeing@ facet, over the scene-level verdict.
newtype WellbeingSel = WellbeingSel Wellbeing

-- | What lets the Today "N to check" badge open exactly the moments it counted. Before
-- this the link degraded to the needs-a-look backlog, a strict superset, so the list was
-- reliably longer than the number that opened it.
instance FromHttpApiData WellbeingSel where
  parseQueryParam t = case lookup t table of
    Just w  -> Right (WellbeingSel w)
    Nothing -> Left (rejection "wellbeing" t wellbeingValues)
    where
      table = [(wellbeingText w, w) | w <- [minBound .. maxBound]]

-- | The complete @wellbeing@ vocabulary, derived from the enum rather than restated.
wellbeingValues :: [Text]
wellbeingValues = map wellbeingText [minBound .. maxBound]

-- | A rejection naming the field, the value given, and the legal set. Every facet reports
-- failure the same way, so a client author never has to guess which spelling a particular
-- parameter wanted.
rejection :: Text -> Text -> [Text] -> Text
rejection field given legal =
  "unrecognised "
    <> field
    <> " value "
    <> T.pack (show given)
    <> "; expected one of "
    <> T.intercalate ", " legal
