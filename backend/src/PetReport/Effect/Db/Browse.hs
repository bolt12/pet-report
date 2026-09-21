-- | The faceted, cursor-paged "browse moments" read, and the one genuinely dynamic query in
-- the persistence layer. A variable set of optional facets plus a keyset cursor assemble a
-- WHERE, built as parameterised fragments so a bind is never interpolated into SQL.
--
-- Facets are EXISTS subqueries over the facts projection and the identity overrides, which
-- returns each observation at most once without a DISTINCT and makes the pet facet reproduce
-- 'subjectStatsBetween's attribution exactly. The row decoder is Queries'.
module PetReport.Effect.Db.Browse
  ( BrowseQuery (..)
  , BrowsePage (..)
  , PetFilter (..)
  , SubjectFilter (..)
  , Behaviour (..)
  , ReviewFilter (..)
  , MediaKind (..)
  , TimeBucket (..)
  , SortDir (..)
  , Cursor (..)
  , encodeCursor
  , decodeCursor
  , browseMoments
  , emptyBrowseQuery
  , escapeLike
  ) where

import           Data.Either                    (partitionEithers)
import           Data.Functor                   ((<&>))
import           Data.Int                       (Int64)
import           Data.Maybe                     (catMaybes, listToMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime)
import           Database.SQLite.Simple         (Only (..), Query (..), SQLData,
                                                 fromOnly, query)
import           Database.SQLite.Simple.ToField (toField)
import           Text.Read                      (readMaybe)

import           PetReport.Domain.Observation   (Observation)
import           PetReport.Domain.Perception    (safetySoundLabels)
import           PetReport.Domain.Types         (Activity, Wellbeing (..),
                                                 activityText, wellbeingText)
import           PetReport.Effect.Db.Handle     (Handle (..), withConn)
import           PetReport.Effect.Db.Queries    (ObsRow, obsRowId, obsRowTs,
                                                 rowToObs)
import           PetReport.Effect.Db.Sql        (placeholders, posixOf)
import           PetReport.Trace                (DbEvent (..), traceWith)

-- --------------------------------------------------------------------------- --
-- Facet vocabulary
-- --------------------------------------------------------------------------- --

data SortDir = Asc | Desc
  deriving stock (Eq, Show)

-- | The media facet, matching 'PetReport.Domain.View.mediaFor'. @audio@ is a 'Heard' sound;
-- @photo@ and @clip@ are 'Seen' scenes, split on source and clip flag.
data MediaKind = MediaPhoto | MediaClip | MediaAudio
  deriving stock (Eq, Show)

-- | The review facet. @NeedsLook@ is the materialised backlog predicate
-- (@needs_look = 1 AND reviewed = 0@), served by the partial needs-look index. Both
-- conjuncts must stay bare, literal and AND-joined, or SQLite stops matching that partial
-- index and silently falls back to a scan.
data ReviewFilter = Reviewed | Unreviewed | NeedsLook
  deriving stock (Eq, Show, Enum, Bounded)

-- | The subject facet: what has to be true of a moment's sightings for it to come back.
--
-- One closed sum rather than a pet field beside a species field, because the two are the
-- same question asked at different precision, and because separate optional fields let a
-- caller ask for a concept the facet cannot express. That is exactly what went wrong: with
-- only a @pet@ slot available, the day summary's "Someone was home" link had to invent a
-- pet id of @visitor@, which resolved to a real-looking filter matching no rows. A moment
-- matches if ANY of its sightings matches, so a frame holding the dog and the sitter is
-- returned by both 'SubjPerson' and a 'SubjPet' naming the dog.
data SubjectFilter
  = SubjPet PetFilter
  | SubjSpecies Text
  | SubjPerson
  | SubjVisiting
  deriving stock (Eq, Show)

-- | The behaviour facet, one constructor per projected behaviour column.
--
-- These columns have existed since the initial schema and were read only through @SUM@ in
-- the statistics queries, never in a predicate. That is why every per-pet tile counted one
-- thing and drilled through to another: a "Meals" tile counts @SUM(ate)@ but the only
-- facet available to its link was @activity@, so it opened @activity=eating@, a different
-- set. Filtering on the same column the tile counts makes the two agree by construction.
data Behaviour
  = BehAte
  | BehDrank
  | BehSlept
  | BehPlayed
  | BehGroomed
  | BehEliminated
  | BehRest
  | BehActive
  | BehConcern
  deriving stock (Eq, Show, Enum, Bounded)

-- | The @observation_subjects@ column a behaviour facet tests. Kept beside the constructor
-- so adding one without wiring its column is a non-exhaustive-match error.
behaviourColumn :: Behaviour -> Text
behaviourColumn b = case b of
  BehAte        -> "ate"
  BehDrank      -> "drank"
  BehSlept      -> "slept"
  BehPlayed     -> "played"
  BehGroomed    -> "groomed"
  BehEliminated -> "eliminated"
  BehRest       -> "rest"
  BehActive     -> "active"
  BehConcern    -> "concern"

-- | A time-of-day window as a half-open second range within a local day
-- @[0, 86400)@. A bucket with @from > to@ wraps midnight (e.g. night).
data TimeBucket = TimeBucket
  { tbFromSec :: !Int
  , tbToSec   :: !Int
  }
  deriving stock (Eq, Show)

-- | The pet facet, pre-resolved by the caller from the loaded profile, which keeps identity
-- resolution out of persistence. Carries the pet id, plus its species when this pet is the
-- unique ACTIVE pet of that species, so an unattributed sighting of the species counts as
-- this pet too. A 'Nothing' species (an ambiguous species, or an archived pet) means only
-- explicit confirmed overrides count.
data PetFilter = PetFilter
  { pfPetId         :: !Text
  , pfUniqueSpecies :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | A keyset cursor on the @(ts, id)@ order the @observations_ts_id@ index serves.
-- @ts@ is POSIX seconds, matching the stored REAL.
data Cursor = Cursor
  { curTs :: !Double
  , curId :: !Int64
  }
  deriving stock (Eq, Show)

-- | A browse request. Every facet is optional. @from@ and @to@ bound a half-open time
-- window, @cameras@ is a room resolved to its cameras, @pet@ is pre-resolved above,
-- @cursor@ pages, @sort@ orders, and @limit@ is the page size.
--
-- @timeOfDay@ carries its tz offset in seconds, making the predicate a pure server-side
-- filter. That fixed offset is an approximation: it lands an hour out only at a DST
-- boundary inside a bucket, which is fine for a facet this coarse.
data BrowseQuery = BrowseQuery
  { bqFrom      :: !(Maybe UTCTime)
  , bqTo        :: !(Maybe UTCTime)
  , bqReview    :: !(Maybe ReviewFilter)
  , bqSubjects  :: ![SubjectFilter]
  , bqCameras   :: !(Maybe [Text])
  , bqActivity  :: !(Maybe Activity)
  , bqBehaviour :: !(Maybe Behaviour)
  , bqWellbeing :: !(Maybe Wellbeing)
  , bqSearch    :: !(Maybe Text)
  , bqMedia     :: !(Maybe MediaKind)
  , bqTimeOfDay :: !(Maybe (TimeBucket, Int))
  , bqSort      :: !SortDir
  , bqCursor    :: !(Maybe Cursor)
  , bqLimit     :: !Int
  }
  deriving stock (Eq, Show)

-- | A browse with every facet off, for a caller to override just the fields it constrains.
-- Named fields rather than positional, so adding a facet cannot silently shift the defaults.
emptyBrowseQuery :: BrowseQuery
emptyBrowseQuery =
  BrowseQuery
    { bqFrom = Nothing
    , bqTo = Nothing
    , bqReview = Nothing
    , bqSubjects = []
    , bqCameras = Nothing
    , bqActivity = Nothing
    , bqBehaviour = Nothing
    , bqWellbeing = Nothing
    , bqSearch = Nothing
    , bqMedia = Nothing
    , bqTimeOfDay = Nothing
    , bqSort = Desc
    , bqCursor = Nothing
    , bqLimit = 50
    }

-- | A page of moments: the items, a cursor to the next page when one exists, and the
-- total matching the facets (only on the first page, where the count is worth paying).
data BrowsePage = BrowsePage
  { bpItems      :: ![Observation]
  , bpNextCursor :: !(Maybe Cursor)
  , bpTotal      :: !(Maybe Int)
  }

-- | An opaque @"ts:id"@ cursor token. Injection-safe: its parts bind as 'SQLData',
-- never as query text. Total to decode (a malformed token yields 'Nothing').
encodeCursor :: Cursor -> Text
encodeCursor (Cursor ts i) = T.pack (show ts) <> ":" <> T.pack (show i)

decodeCursor :: Text -> Maybe Cursor
decodeCursor t = case T.splitOn ":" t of
  [a, b] -> Cursor <$> readMaybe (T.unpack a) <*> readMaybe (T.unpack b)
  _      -> Nothing

-- --------------------------------------------------------------------------- --
-- The query
-- --------------------------------------------------------------------------- --

-- | Run a browse request: one page of decoded moments plus paging state. Fetches
-- @limit + 1@ rows to detect a next page. An undecodable row is warned about and skipped,
-- while the next cursor comes from the last raw row, so a bad row can never truncate paging.
-- The facet total is computed only on the first, cursorless page.
browseMoments :: Handle -> BrowseQuery -> IO BrowsePage
browseMoments h bq = withConn h $ \c -> do
  let base = Just (readableRow, []) : baseFacets bq
      (whereBase, bindsBase) = assembleWhere base
      (whereFull, bindsFull) = assembleWhere (base ++ [cursorFacet (bqSort bq) (bqCursor bq)])
      dir = case bqSort bq of Asc -> "ASC"; Desc -> "DESC"
      pageSql =
        Query $
          selectCols
            <> " FROM observations o WHERE "
            <> whereFull
            <> " ORDER BY o.ts " <> dir <> ", o.id " <> dir
            <> " LIMIT ?"
      pageBinds = bindsFull ++ [toField (bqLimit bq + 1)]
  rows <- query c pageSql pageBinds :: IO [ObsRow]
  let n = max 0 (bqLimit bq)
      pageRows = take n rows
      -- Only claim a next page when this one actually returned rows to page from. A zero
      -- or negative limit yields no page, and so no dead-end "more, but no cursor".
      hasMore = not (null pageRows) && length rows > n
      (bad, items) = partitionEithers (map rowToObs pageRows)
  mapM_ (traceWith (hTracer h) . UnreadableRow "browse" . T.pack) bad
  -- The next cursor comes from the last RETURNED raw row, reading ts and id directly with
  -- no decode, so a row that fails to decode can never silently truncate a page.
  let nextC =
        if hasMore
          then case reverse pageRows of
            (r : _) -> Just (Cursor (obsRowTs r) (obsRowId r))
            []      -> Nothing
          else Nothing
  total <- case bqCursor bq of
    Just _ -> pure Nothing -- the count is stable across a browse; only pay it once
    Nothing -> do
      cnt <-
        query
          c
          (Query ("SELECT COUNT(*) FROM observations o WHERE " <> whereBase))
          bindsBase ::
          IO [Only Int]
      pure (Just (maybe 0 fromOnly (listToMaybe cnt)))
  pure (BrowsePage items nextC total)

-- | The rows 'PetReport.Effect.Db.Queries.rowToObs' can actually decode, as a predicate both
-- the page and the count carry.
--
-- The page partitions undecodable rows out and traces them; the count did not know about
-- them, so a corrupt row left the header saying "20 moments" above a list of 19, with
-- nothing on screen to explain the difference and no page that would ever produce the
-- twentieth. Structural corruption is what the decoder rejects, and it is expressible here;
-- a blob that parses as SQL JSON but not as a 'Perception' still slips past, and is still
-- traced.
readableRow :: Text
readableRow =
  "(o.source = 'sample' \
  \ OR (o.source = 'event' AND o.event_id IS NOT NULL AND o.label IS NOT NULL AND o.score IS NOT NULL)) \
  \AND json_valid(o.perception)"

-- | The o.-qualified projection, in the exact column order 'rowToObs' decodes.
selectCols :: Text
selectCols =
  "SELECT o.id, o.ts, o.camera, o.source, o.event_id, o.label, o.score, \
  \o.perception, o.reviewed, o.has_clip, o.has_snapshot"

-- | Combine the present facets with AND, in bind order; an empty set matches all.
assembleWhere :: [Maybe (Text, [SQLData])] -> (Text, [SQLData])
assembleWhere fs = case catMaybes fs of
  [] -> ("1 = 1", [])
  xs -> (T.intercalate " AND " (map fst xs), concatMap snd xs)

-- | Every facet except the cursor, each a fragment plus its ordered binds.
baseFacets :: BrowseQuery -> [Maybe (Text, [SQLData])]
baseFacets bq =
  [ bqFrom bq <&> \t -> ("o.ts >= ?", [toField (posixOf t)])
  , bqTo bq <&> \t -> ("o.ts < ?", [toField (posixOf t)])
  , bqReview bq <&> \case
      Reviewed -> ("o.reviewed = 1", [])
      Unreviewed -> ("o.reviewed = 0", [])
      NeedsLook -> ("o.needs_look = 1 AND o.reviewed = 0", [])
  , bqCameras bq <&> \cams -> case cams of
      [] -> ("0 = 1", [])
      _ -> ("o.camera IN " <> placeholders (length cams), map toField cams)
  , bqActivity bq <&> \act ->
      ( "EXISTS (SELECT 1 FROM observation_subjects s WHERE s.obs_id = o.id AND s.activity = ?)"
      , [toField (activityText act)]
      )
  -- The column name comes from 'behaviourColumn' over a closed enum, never from the
  -- request, so this stays as free of interpolated caller input as every other fragment.
  , bqBehaviour bq <&> \b ->
      ( "EXISTS (SELECT 1 FROM observation_subjects s \
        \WHERE s.obs_id = o.id AND s." <> behaviourColumn b <> " = 1)"
      , []
      )
  -- Scene wellbeing lives in the perception blob and has no column of its own. Reading it
  -- with json_extract is what migration 2 already does to backfill needs_look, and it is
  -- what lets the Today "N to check" badge open exactly the moments it counted rather than
  -- the wider needs-a-look backlog. Unindexed, which is fine at household scale.
  , bqWellbeing bq <&> \wb ->
      let sceneArm = ("json_extract(o.perception, '$.scene.wellbeing') = ?", [toField (wellbeingText wb)])
       in case wb of
            -- A safety sound has no scene, so json_extract reads NULL and the arm above can
            -- never match it, yet 'PetReport.Domain.Stats.wellbeingOf' calls it concerning
            -- and the card shows it as such. The Today "N to check" badge counted those and
            -- then opened a shorter list. Both readers now select the same sounds, off the
            -- one label list they share.
            Concerning ->
              ( "(" <> fst sceneArm <> " OR json_extract(o.perception, '$.sound') IN " <> placeholders (length safetySoundLabels) <> ")"
              , snd sceneArm ++ map toField safetySoundLabels
              )
            -- Every other wellbeing is a scene's own word; a sound carries none at all, so
            -- widening these would select sounds no card ever labels that way.
            _ -> sceneArm
  -- Free-text search: an approximate match over the moment's description, subjects,
  -- species, activity and sound text, all of which live in the perception blob. Room has
  -- its own facet. Crude, but it holds up at household scale.
  , bqSearch bq <&> \q ->
      -- ESCAPE, because LIKE reads % and _ as wildcards and the box takes whatever the owner
      -- types. Without it "100%" matched every moment and "_" matched all of them too, which
      -- reads as a broken search rather than as a syntax nobody was told about.
      ("o.perception LIKE ? ESCAPE '\\'", [toField ("%" <> escapeLike q <> "%")])
  , bqMedia bq <&> \case
      -- Mirrors 'PetReport.Domain.View.mediaFor', which dispatches on the perception: a
      -- 'Heard' sound is audio, a 'Seen' scene is photo or clip. So the facet keys off the
      -- blob's @kind@ discriminator rather than the presence of subject rows. A scene with
      -- zero appearances writes no rows yet is still a photo, and a sound that saved a
      -- clip is still audio.
      MediaPhoto ->
        ( "json_extract(o.perception, '$.kind') = 'scene' \
          \AND (o.source = 'sample' OR (o.source = 'event' AND COALESCE(o.has_clip, 0) = 0))"
        , []
        )
      MediaClip ->
        ( "json_extract(o.perception, '$.kind') = 'scene' \
          \AND o.source = 'event' AND (o.has_clip IS NULL OR o.has_clip = 1)"
        , []
        )
      MediaAudio ->
        ("json_extract(o.perception, '$.kind') = 'sound'", [])
  , bqTimeOfDay bq <&> \(TimeBucket a b, off) ->
      -- Local second-of-day. o.ts is UTC POSIX and local = UTC + off, so the offset is
      -- ADDED. The extra @+ 86400) % 86400@ keeps a west-of-UTC negative offset in range.
      let e = "((CAST(o.ts AS INTEGER) + ?) % 86400 + 86400) % 86400"
       in if a <= b
            then (e <> " >= ? AND " <> e <> " < ?", [toField off, toField a, toField off, toField b])
            else ("(" <> e <> " >= ? OR " <> e <> " < ?)", [toField off, toField a, toField off, toField b])
  ]
    -- One fragment per subject asked for, AND-ed with the rest by 'assembleWhere'. A
    -- moment must satisfy EVERY subject named, so "Mochi and a person" returns the frames
    -- that hold both rather than either.
    ++ map (Just . subjectFacet) (bqSubjects bq)

-- | Neutralise the LIKE wildcards in owner-typed search text, so it matches literally.
-- The escape character itself goes first, or escaping the others would double-escape it.
escapeLike :: Text -> Text
escapeLike = T.replace "%" "\\%" . T.replace "_" "\\_" . T.replace "\\" "\\\\"

-- | The EXISTS fragment for one subject filter.
subjectFacet :: SubjectFilter -> (Text, [SQLData])
subjectFacet = \case
  SubjPet pf -> case pfUniqueSpecies pf of
    Just sp ->
      ( "EXISTS (SELECT 1 FROM observation_subjects s \
        \LEFT JOIN subject_identity si ON si.obs_id = s.obs_id AND si.seq = s.seq AND si.confirmed = 1 \
        \WHERE s.obs_id = o.id AND s.is_person = 0 \
        \AND (si.pet_id = ? OR (si.pet_id IS NULL AND COALESCE(si.visiting, 0) = 0 AND s.species = ?)))"
      , [toField (pfPetId pf), toField sp]
      )
    Nothing ->
      ( "EXISTS (SELECT 1 FROM subject_identity si WHERE si.obs_id = o.id AND si.confirmed = 1 AND si.pet_id = ?)"
      , [toField (pfPetId pf)]
      )
  SubjSpecies sp ->
    ( "EXISTS (SELECT 1 FROM observation_subjects s WHERE s.obs_id = o.id AND s.species = ?)"
    , [toField sp]
    )
  -- The facet the tree was missing. @is_person@ has been stored on every projected
  -- sighting since the initial schema and was only ever read as an exclusion
  -- (@is_person = 0@) inside the pet facet, so "show me when someone was home" had no
  -- way to be asked. No new index: the subquery is keyed by obs_id, which is the
  -- leading column of the observation_subjects primary key.
  SubjPerson ->
    ("EXISTS (SELECT 1 FROM observation_subjects s WHERE s.obs_id = o.id AND s.is_person = 1)", [])
  -- An animal the owner marked as not theirs. Reads the override table directly, since
  -- visiting-ness is an owner assertion and never a model output.
  SubjVisiting ->
    ( "EXISTS (SELECT 1 FROM subject_identity si \
      \WHERE si.obs_id = o.id AND si.confirmed = 1 AND si.visiting = 1)"
    , []
    )

-- | The keyset predicate for the cursor, matching the sort direction.
cursorFacet :: SortDir -> Maybe Cursor -> Maybe (Text, [SQLData])
cursorFacet _ Nothing = Nothing
cursorFacet Asc (Just (Cursor ts i)) =
  Just ("(o.ts > ? OR (o.ts = ? AND o.id > ?))", [toField ts, toField ts, toField i])
cursorFacet Desc (Just (Cursor ts i)) =
  Just ("(o.ts < ? OR (o.ts = ? AND o.id < ?))", [toField ts, toField ts, toField i])
