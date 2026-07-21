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
  , ReviewFilter (..)
  , MediaKind (..)
  , TimeBucket (..)
  , SortDir (..)
  , Cursor (..)
  , encodeCursor
  , decodeCursor
  , browseMoments
  , emptyBrowseQuery
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
-- (@uncertain = 1 AND reviewed = 0@), served by the partial needs-look index.
data ReviewFilter = Reviewed | Unreviewed | NeedsLook
  deriving stock (Eq, Show)

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
  , bqPet       :: !(Maybe PetFilter)
  , bqCameras   :: !(Maybe [Text])
  , bqSpecies   :: !(Maybe Text)
  , bqActivity  :: !(Maybe Text)
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
    , bqPet = Nothing
    , bqCameras = Nothing
    , bqSpecies = Nothing
    , bqActivity = Nothing
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
  let base = baseFacets bq
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
      NeedsLook -> ("o.uncertain = 1 AND o.reviewed = 0", [])
  , bqCameras bq <&> \cams -> case cams of
      [] -> ("0 = 1", [])
      _ -> ("o.camera IN " <> placeholders (length cams), map toField cams)
  , bqSpecies bq <&> \sp ->
      ( "EXISTS (SELECT 1 FROM observation_subjects s WHERE s.obs_id = o.id AND s.species = ?)"
      , [toField sp]
      )
  , bqActivity bq <&> \act ->
      ( "EXISTS (SELECT 1 FROM observation_subjects s WHERE s.obs_id = o.id AND s.activity = ?)"
      , [toField act]
      )
  -- Free-text search: an approximate match over the moment's description, subjects,
  -- species, activity and sound text, all of which live in the perception blob. Room has
  -- its own facet. Crude, but it holds up at household scale.
  , bqSearch bq <&> \q ->
      ("o.perception LIKE ?", [toField ("%" <> q <> "%")])
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
  , bqPet bq <&> \pf -> case pfUniqueSpecies pf of
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
  ]

-- | The keyset predicate for the cursor, matching the sort direction.
cursorFacet :: SortDir -> Maybe Cursor -> Maybe (Text, [SQLData])
cursorFacet _ Nothing = Nothing
cursorFacet Asc (Just (Cursor ts i)) =
  Just ("(o.ts > ? OR (o.ts = ? AND o.id > ?))", [toField ts, toField ts, toField i])
cursorFacet Desc (Just (Cursor ts i)) =
  Just ("(o.ts < ? OR (o.ts = ? AND o.id < ?))", [toField ts, toField ts, toField i])
