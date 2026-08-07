-- | Every read and write against the pool: profiles, observations and their facts
-- projection, corrections/edits, state and reports, keepsakes, and the
-- per-pet summary cache. The bulk of the effect; the handle, schema, and shared
-- facts writer live in the sibling modules this one imports.
module PetReport.Effect.Db.Queries
  ( getProfile
  , putProfile
  , modifyProfile
  , modifyProfileE
  , insertObservation
  , observationsBetween
  , getObservation
  , getObservationsByIds
  , overridesBetween
  , overridesForObs
  , overridesForObsIds
  , subjectStatsBetween
  , markReviewed
  , deleteObservation
  , deleteObservations
  , purgePetAndProfile
  , getState
  , setState
  , getIngestWatermark
  , setIngestWatermark
  , getDaySwept
  , setDaySwept
  , insertReport
  , reportExists
  , latestReport
  , eventStored
  , recentEventStarts
  , correctObservation
  , revertObservation
  , editObservation
  , reprojectAll
  , correctionStats
  , tsRange
  , obsEventId
  , setTranscript
  , transcriptsFor
  , Keepsake (..)
  , insertKeepsake
  , listKeepsakes
  , keptObsIds
  , keptSampleStamps
  , deleteKeepsake
  , PetSummary (..)
  , getPetSummary
  , putPetSummary
  , schemaVersion
    -- Low-level row helpers shared with "PetReport.Effect.Db.Browse".
  , ObsRow
  , rowToObs
  , obsRowId
  , obsRowTs
  ) where

import           Control.Monad                  (forM_, when)
import           Data.Aeson                     (ToJSON (..), decodeStrict,
                                                 eitherDecodeStrict,
                                                 genericToJSON)
import           Data.Aeson.Text                (encodeToLazyText)
import           Data.Either                    (partitionEithers)
import           Data.Int                       (Int64)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe, isJust,
                                                 listToMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (encodeUtf8)
import qualified Data.Text.Lazy                 as TL
import           Data.Time                      (Day, UTCTime, getCurrentTime)
import           Data.Time.Calendar             (showGregorian)
import           Database.SQLite.Simple         (Connection, Only (..),
                                                 Query (..), changes, execute,
                                                 execute_, fromOnly,
                                                 lastInsertRowId, query, query_,
                                                 withImmediateTransaction,
                                                 (:.) (..))
import           Database.SQLite.Simple.FromRow (FromRow (..))
import           GHC.Generics                   (Generic)
import           Text.Read                      (readMaybe)

import           PetReport.Domain.Observation   (FrigateMeta (..),
                                                 NewObservation (..),
                                                 Observation (..), Origin (..),
                                                 originMeta)
import           PetReport.Domain.Perception    (Correction (..), Perception,
                                                 SceneEdit, applyCorrection,
                                                 applyEdit)
import           PetReport.Domain.PetReport     (WellbeingKind,
                                                 wellbeingKindFromText,
                                                 wellbeingKindText)
import           PetReport.Domain.Profile       (Overrides, Profile,
                                                 SubjectId (..), emptyProfile)
import           PetReport.Domain.Report        (Period (..), Report (..),
                                                 periodText)
import           PetReport.Domain.Stats         (PetStat (..), SubjectFact (..),
                                                 SubjectKey (..), factsOf,
                                                 isUncertain)
import           PetReport.Domain.Types         (Camera (..), EventId (..),
                                                 ObsId (..), PetId (..),
                                                 Species (..), cameraText,
                                                 petIdText)
import           PetReport.Effect.Db.Facts      (boolToInt, confOf,
                                                 decodePerception, writeFacts)
import           PetReport.Effect.Db.Handle     (Handle (..), withConn)
import           PetReport.Effect.Db.Migrations (currentVersion)
import           PetReport.Effect.Db.Sql        (fromPosix, placeholders, posixOf)
import           PetReport.Util                 (prefixed)
import           PetReport.Trace                (DbEvent (..), Tracer, traceWith)

-- Every WRITE transaction uses 'withImmediateTransaction' (BEGIN IMMEDIATE), not
-- sqlite-simple's 'withTransaction' (BEGIN DEFERRED). A DEFERRED transaction that reads
-- then writes takes its write lock late, so it can fail SQLITE_BUSY_SNAPSHOT if another
-- connection wrote in between, and that is an error busy_timeout does NOT retry. IMMEDIATE
-- takes the write lock up front, where busy_timeout can wait it out.

-- | Per-camera correction counts as @(camera, total, corrected)@, where corrected means the
-- stored perception has diverged from the model's original reading in @raw_perception@.
-- Feeds the prompt-tuning loop.
correctionStats :: Handle -> IO [(Text, Int, Int)]
correctionStats h = withConn h $ \c ->
  query_
    c
    "SELECT camera, COUNT(*), \
    \SUM(CASE WHEN raw_perception IS NOT NULL AND raw_perception <> perception THEN 1 ELSE 0 END) \
    \FROM observations GROUP BY camera"

-- | The first and last observation timestamps, so a caller can bound the day navigator at
-- the earliest logged day. 'Nothing' on an empty table, where the aggregate returns a single
-- all-null row.
tsRange :: Handle -> IO (Maybe (UTCTime, UTCTime))
tsRange h = withConn h $ \c -> do
  rows <- query_ c "SELECT MIN(ts), MAX(ts) FROM observations" :: IO [(Maybe Double, Maybe Double)]
  pure $ case rows of
    ((Just lo, Just hi) : _) -> Just (fromPosix lo, fromPosix hi)
    _                        -> Nothing

-- | The Frigate event id backing an observation, if any (sample rows have none).
obsEventId :: Handle -> Int64 -> IO (Maybe Text)
obsEventId h oid = withConn h $ \c -> do
  rows <- query c "SELECT event_id FROM observations WHERE id = ?" (Only oid) :: IO [Only (Maybe Text)]
  pure (listToMaybe rows >>= fromOnly)

-- | Cache a transcript against an observation (a no-op if the id is unknown).
setTranscript :: Handle -> Int64 -> Text -> IO ()
setTranscript h oid txt = withConn h $ \c ->
  execute c "UPDATE observations SET transcript = ? WHERE id = ?" (txt, oid)

-- | The cached transcripts for observations in @[lo, hi)@, keyed by id, so the day view can
-- show what a speech moment said. Rows without a transcript are omitted.
transcriptsFor :: Handle -> UTCTime -> UTCTime -> IO (Map ObsId Text)
transcriptsFor h lo hi = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT id, transcript FROM observations \
      \WHERE ts >= ? AND ts < ? AND transcript IS NOT NULL"
      (posixOf lo, posixOf hi) ::
      IO [(Int64, Text)]
  pure (Map.fromList [(ObsId i, t) | (i, t) <- rows])

-- | Force a full rebuild of the facts projection from every observation (for the
-- @reproject@ CLI, e.g. after the projection logic changes). Returns the count.
reprojectAll :: Handle -> IO Int
reprojectAll h = withConn h $ \c -> withImmediateTransaction c $ do
  rows <- query_ c "SELECT id, perception FROM observations" :: IO [(Int64, Text)]
  forM_ rows $ \(oid, perc) ->
    case decodePerception perc of
      Right p -> do
        writeFacts c oid p
        -- Rebuild the derived per-observation signals too, so a reproject fully applies a
        -- projection-logic or threshold change rather than only the facts.
        execute
          c
          "UPDATE observations SET confidence = ?, uncertain = ? WHERE id = ?"
          (confOf p, boolToInt (isUncertain p), oid)
      Left _  -> pure ()
  pure (length rows)

-- | The @user_version@ the pool's database is on, for a test or a diagnostic to confirm
-- migrations ran to 'PetReport.Effect.Db.Migrations.latestSchemaVersion'.
schemaVersion :: Handle -> IO Int
schemaVersion h = withConn h currentVersion

-- --------------------------------------------------------------------------- --
-- Profile
-- --------------------------------------------------------------------------- --

getProfile :: Handle -> IO Profile
getProfile h = withConn h (getProfileC (hTracer h))

-- | Read the profile on an already-held connection, so a read-modify-write can run on that
-- same connection inside one transaction. Mirrors 'getObservationC'.
getProfileC :: Tracer IO DbEvent -> Connection -> IO Profile
getProfileC tracer c = do
  rows <- query c "SELECT v FROM settings WHERE k = ?" (Only ("profile" :: Text))
  case rows of
    (Only t : _) -> case eitherDecodeStrict (encodeUtf8 t) of
      Right p -> pure p
      Left e -> do
        traceWith tracer (CorruptProfileJson (T.pack e))
        pure emptyProfile
    _ -> pure emptyProfile

putProfile :: Handle -> Profile -> IO ()
putProfile h p = withConn h (`putProfileC` p)

-- | Write the profile on an already-held connection (the RMW companion to
-- 'getProfileC').
putProfileC :: Connection -> Profile -> IO ()
putProfileC c p =
  execute
    c
    "INSERT INTO settings (k, v) VALUES ('profile', ?) \
    \ON CONFLICT(k) DO UPDATE SET v = excluded.v"
    (Only (TL.toStrict (encodeToLazyText p)))

-- | Atomically read-modify-write the profile on one connection, returning the new value.
-- The per-pet roster edits (add, edit, archive) commit as one act this way, so two
-- overlapping edits serialize instead of clobbering each other.
modifyProfile :: Handle -> (Profile -> Profile) -> IO Profile
modifyProfile h f = withConn h $ \c -> withImmediateTransaction c $ do
  prof <- getProfileC (hTracer h) c
  let prof' = f prof
  putProfileC c prof'
  pure prof'

-- | 'modifyProfile' where the change may be rejected, say for a duplicate pet id. The check
-- and the write share one BEGIN IMMEDIATE transaction, so a uniqueness test cannot race a
-- concurrent add. On 'Left' the profile is left untouched.
modifyProfileE :: Handle -> (Profile -> Either e Profile) -> IO (Either e Profile)
modifyProfileE h f = withConn h $ \c -> withImmediateTransaction c $ do
  prof <- getProfileC (hTracer h) c
  case f prof of
    Left e -> pure (Left e)
    Right prof' -> do
      putProfileC c prof'
      pure (Right prof')

-- --------------------------------------------------------------------------- --
-- Observations
-- --------------------------------------------------------------------------- --

-- | Store a new observation and its facts projection in one transaction. @INSERT OR
-- IGNORE@ against the unique event-id index makes re-storing an already-seen Frigate
-- event a silent no-op. A sample row has a NULL event id, so it never conflicts. The facts
-- are written only when the row actually landed.
insertObservation :: Handle -> NewObservation -> IO ()
insertObservation h obs = withConn h $ \c -> withImmediateTransaction c $ do
  let perc = TL.toStrict (encodeToLazyText (noPerception obs))
      -- The Frigate meta an event carries, Nothing for a sample. Every stored column
      -- projects off this one Maybe, so "a sample has no meta" is stated once rather than
      -- re-cased per column.
      m = originMeta (noOrigin obs)
  execute
    c
    "INSERT OR IGNORE INTO observations \
    \(ts, camera, source, event_id, label, score, perception, reviewed, confidence, raw_perception, uncertain, has_clip, has_snapshot) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( ( posixOf (noAt obs)
      , cameraText (noCamera obs)
      , maybe "sample" (const "event") m :: Text
      , fmap (\fm -> case eventId fm of EventId e -> e) m
      , fmap detectorLabel m
      , fmap detectorScore m
      , perc
      -- A newly-ingested row is always unreviewed. 'NewObservation' carries no review
      -- flag, so this is 0 by construction rather than a value read off the input.
      , 0 :: Int
      , confOf (noPerception obs)
      , perc
      )
        :. ( boolToInt (isUncertain (noPerception obs))
           , fmap (\fm -> if detectorHasClip fm then 1 else 0 :: Int) m
           , fmap (\fm -> if detectorHasSnapshot fm then 1 else 0 :: Int) m
           )
    )
  n <- changes c
  -- The row and its facts projection commit together, so a crash between them cannot
  -- leave an observation the stats queries silently under-count.
  when (n > 0) $ do
    oid <- lastInsertRowId c
    writeFacts c oid (noPerception obs)

-- | Observations with @from <= ts < to@, chronological.
observationsBetween :: Handle -> UTCTime -> UTCTime -> IO [Observation]
observationsBetween h lo hi = withConn h $ \c -> do
  rows <-
    query
      c
      (obsSelect <> " WHERE ts >= ? AND ts < ? ORDER BY ts ASC")
      (posixOf lo, posixOf hi)
  let (bad, good) = partitionEithers (map rowToObs rows)
  mapM_ (traceWith (hTracer h) . UnreadableRow "read" . T.pack) bad
  pure good

-- | Per-subject aggregate stats over @[lo, hi)@, straight from the facts projection, which
-- stays cheap over long windows because nothing decodes a blob. Person rows are excluded.
--
-- An appearance the owner reassigned to a specific pet is keyed by that pet whatever its
-- species, a visiting animal is dropped, and everything else stays keyed by species for the
-- caller's unique-species rule. This is 'identifyWith' expressed relationally, so it agrees
-- with the blob-path 'presence' over the same window.
subjectStatsBetween :: Handle -> UTCTime -> UTCTime -> IO (Map SubjectKey PetStat)
subjectStatsBetween h lo hi = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT os.species, si.pet_id, COUNT(*), SUM(os.rest), SUM(os.active), SUM(os.ate), \
      \SUM(os.drank), SUM(os.slept), SUM(os.played), SUM(os.groomed), SUM(os.eliminated), SUM(os.concern) \
      \FROM observation_subjects os JOIN observations o ON o.id = os.obs_id \
      \LEFT JOIN subject_identity si ON si.obs_id = os.obs_id AND si.seq = os.seq AND si.confirmed = 1 \
      \WHERE os.is_person = 0 AND os.species IS NOT NULL AND o.ts >= ? AND o.ts < ? \
      \AND COALESCE(si.visiting, 0) = 0 \
      \GROUP BY os.species, si.pet_id"
      (posixOf lo, posixOf hi)
  pure $
    Map.fromListWith
      (<>)
      [ (key, PetStat sight rest act ate drank slept played groomed elim concern)
      | AggRow sp mpid sight rest act ate drank slept played groomed elim concern <- rows
      , let key = maybe (KSpecies (Species sp)) (KPet . PetId) mpid
      ]

data AggRow
  = AggRow !Text !(Maybe Text) !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
  deriving stock (Generic)
  deriving anyclass (FromRow)

-- | Confirmed owner identity overrides for observations in @[lo, hi)@, keyed by
-- @(obs_id, seq)@ for the blob read path. Provisional auto-IDs are excluded, so a machine
-- guess never silently steers the day view or the stats.
overridesBetween :: Handle -> UTCTime -> UTCTime -> IO Overrides
overridesBetween h lo hi = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT si.obs_id, si.seq, si.pet_id, si.visiting \
      \FROM subject_identity si JOIN observations o ON o.id = si.obs_id \
      \WHERE si.confirmed = 1 AND o.ts >= ? AND o.ts < ?"
      (posixOf lo, posixOf hi)
  pure (Map.fromList (map toOverride rows))

-- | Confirmed owner identity overrides for a single observation. Same shape as
-- 'overridesBetween', for the single-moment and keepsake read paths, which have an id rather
-- than a time window.
overridesForObs :: Handle -> Int64 -> IO Overrides
overridesForObs h oid = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT obs_id, seq, pet_id, visiting FROM subject_identity \
      \WHERE confirmed = 1 AND obs_id = ?"
      (Only oid)
  pure (Map.fromList (map toOverride rows))

-- | Confirmed owner identity overrides for a SET of observations in ONE query. The id-set
-- companion to 'overridesForObs', for enriching moments that share no time window, as
-- keepsakes do. An empty id list short-circuits, since an empty @IN ()@ is not valid SQL.
overridesForObsIds :: Handle -> [Int64] -> IO Overrides
overridesForObsIds _ [] = pure Map.empty
overridesForObsIds h oids = withConn h $ \c -> do
  rows <-
    query
      c
      ( "SELECT obs_id, seq, pet_id, visiting FROM subject_identity \
        \WHERE confirmed = 1 AND obs_id IN "
          <> inPlaceholders (length oids)
      )
      oids
  pure (Map.fromList (map toOverride rows))

-- | 'placeholders' lifted to 'Query'. Callers guard @n >= 1@, so this never renders an
-- empty @()@.
inPlaceholders :: Int -> Query
inPlaceholders = Query . placeholders

-- | Decode a @subject_identity@ row into an override entry (a specific pet, or a
-- visiting animal). Shared by the windowed and single-observation reads.
toOverride :: (Int64, Int, Maybe Text, Int) -> ((Int64, Int), SubjectId)
toOverride (oid, s, mpid, vis) =
  let sid = case mpid of
        Just p | vis == (0 :: Int) -> IdPet (PetId p)
        _                          -> IdVisiting
   in ((oid, s), sid)

-- | A raw observation row, one field per SELECT column in order. Named fields with a
-- derived 'FromRow', so the column order lives in the record rather than in a hand-written
-- @field@ chain.
data ObsRow = ObsRow
  { orId       :: Int64
  , orTs       :: Double
  , orCamera   :: Text
  , orSource   :: Text
  , orEventId  :: Maybe Text
  , orLabel    :: Maybe Text
  , orScore    :: Maybe Double
  , orPerc     :: Text
  , orReviewed :: Int
  , orHasClip  :: Maybe Int
  , orHasSnap  :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (FromRow)

-- | Decode a raw row into a domain 'Observation', or 'Left' a reason it is corrupt:
-- unreadable perception JSON, or an event row with impossible NULL meta. Callers drop the
-- 'Left' rows, and the windowed reads trace them, so one bad row never fails a whole read.
rowToObs :: ObsRow -> Either String Observation
rowToObs (ObsRow i ts cam src eid lbl scr perc rev hasClip hasSnap) = do
  p <- decodePerception perc
  org <- case src of
    "sample" -> Right PeriodicSample
    -- The insert writes label, score and event_id together, so a NULL in any of them on
    -- an event row is corruption rather than a legitimate absence. Fail the decode instead
    -- of inventing "" or 0: the caller partitions the Left rows out and traces them, so a
    -- corrupt row surfaces rather than showing up with a blank detector label. A
    -- PeriodicSample legitimately has NULL meta, but never reaches this branch.
    "event" -> case (eid, lbl, scr) of
      -- A NULL has_clip or has_snapshot means the event was stored before that column
      -- existed, so assume present rather than hiding the media. An explicit 0 means
      -- Frigate genuinely had none, and the view drops that URL rather than serving one
      -- that 404s.
      (Just e, Just l, Just s) -> Right (FromEvent (FrigateMeta (EventId e) l s (maybe True (/= 0) hasClip) (maybe True (/= 0) hasSnap)))
      (Nothing, _, _)          -> Left "event row without event_id"
      _                        -> Left "event row with NULL label or score"
    other -> Left ("unknown source: " <> T.unpack other)
  pure
    Observation
      { obsId = ObsId i
      , at = fromPosix ts
      , camera = Camera cam
      , origin = org
      , perception = p
      , reviewed = rev /= 0
      }

-- | The base observation projection shared by every read, so its column order
-- stays in lockstep with 'rowToObs'. Callers append their own WHERE/ORDER clause.
obsSelect :: Query
obsSelect =
  "SELECT id, ts, camera, source, event_id, label, score, perception, reviewed, has_clip, has_snapshot \
  \FROM observations"

-- | The row id from a raw 'ObsRow', for delete-by-id without a full decode.
obsRowId :: ObsRow -> Int64
obsRowId = orId

-- | The timestamp (POSIX seconds) from a raw 'ObsRow', for the keyset cursor in
-- "PetReport.Effect.Db.Browse" without a full decode.
obsRowTs :: ObsRow -> Double
obsRowTs = orTs

-- | Mark observations reviewed by id, stamping the review time. The per-id UPDATEs commit
-- together, so a crash mid-batch cannot leave half a "mark all reviewed" applied.
markReviewed :: Handle -> [Int64] -> IO ()
markReviewed h ids = do
  now <- getCurrentTime
  withConn h $ \c -> withImmediateTransaction c $
    mapM_
      (\i -> execute c "UPDATE observations SET reviewed = 1, reviewed_at = ? WHERE id = ?" (posixOf now, i))
      ids

getObservation :: Handle -> Int64 -> IO (Maybe Observation)
getObservation h oid = withConn h (`getObservationC` oid)

-- | Read one observation on an already-held connection, so a correction can do
-- its read and write on the same connection inside one transaction.
getObservationC :: Connection -> Int64 -> IO (Maybe Observation)
getObservationC c oid = do
  rows <-
    query
      c
      (obsSelect <> " WHERE id = ?")
      (Only oid)
  pure $ case rows of
    (r : _) -> either (const Nothing) Just (rowToObs r)
    _       -> Nothing

-- | Read many observations in ONE query, keyed by id, so a caller enriching a list of
-- moments does not fire a pooled single-row read per id. An id whose row is absent or fails
-- to decode is missing from the map, and the caller drops it exactly as the single-row path
-- would. An empty id list short-circuits.
getObservationsByIds :: Handle -> [Int64] -> IO (Map Int64 Observation)
getObservationsByIds _ [] = pure Map.empty
getObservationsByIds h oids = withConn h $ \c -> do
  rows <- query c (obsSelect <> " WHERE id IN " <> inPlaceholders (length oids)) oids
  let (bad, good) = partitionEithers (map rowToObs rows)
  mapM_ (traceWith (hTracer h) . UnreadableRow "read" . T.pack) bad
  pure (Map.fromList [(i, o) | o <- good, let ObsId i = obsId o])

-- | Delete an observation, returning the deleted row so the caller can clean up its proof
-- frame. The event watermark is deliberately left untouched.
deleteObservation :: Handle -> Int64 -> IO (Maybe Observation)
deleteObservation h oid = do
  m <- getObservation h oid
  -- The keepsake, facts and identity rows all cascade from the FK, foreign_keys being ON
  -- per connection, so this cannot leave a keepsake dangling with a dead thumbnail.
  withConn h $ \c ->
    execute c "DELETE FROM observations WHERE id = ?" (Only oid)
  pure m

-- | Bulk-delete observations, either all of them on 'Nothing' or those before a cutoff, and
-- return the deleted rows so the caller can clean up their proof frames. Their keepsakes,
-- facts and identity overrides cascade via FK.
deleteObservations :: Handle -> Maybe UTCTime -> IO [Observation]
deleteObservations h mcut = withConn h $ \c -> withImmediateTransaction c $ do
  rows <- case mcut of
    Just cut ->
      query
        c
        (obsSelect <> " WHERE ts < ?")
        (Only (posixOf cut))
    Nothing -> query_ c obsSelect
  let (_, good) = partitionEithers (map rowToObs rows)
  case mcut of
    Just cut -> execute c "DELETE FROM observations WHERE ts < ?" (Only (posixOf cut))
    Nothing  -> execute_ c "DELETE FROM observations"
  pure good

-- | Permanently erase a pet AND drop it from the profile, in one transaction on one
-- connection. The moments tied to it, its keepsakes, its cached summary, and the profile
-- read-modify-write all commit together, so a crash cannot erase the data yet leave the pet
-- listed, or the reverse. Returns the deleted observations so the caller can free their
-- proof frames after the commit.
--
-- A moment the pet was only auto-identified in carries no stored link to it, so it stays
-- and reverts to species-level. A moment shared with another pet is deleted whole. This is
-- the path behind deleting an archived pet's data for good.
purgePetAndProfile :: Handle -> Text -> (Profile -> Profile) -> IO [Observation]
purgePetAndProfile h pid updateProfile = withConn h $ \c -> withImmediateTransaction c $ do
  deleted <- purgePetBody (hTracer h) c pid
  prof <- getProfileC (hTracer h) c
  putProfileC c (updateProfile prof)
  pure deleted

-- | The pet-erase body, on an already-held connection so the spanning transaction
-- can also run the profile read-modify-write on the same connection.
purgePetBody :: Tracer IO DbEvent -> Connection -> Text -> IO [Observation]
purgePetBody tracer c pid = do
  -- The pet's moments: those it is a confirmed subject of, plus those kept as it, minus
  -- any kept moment since re-corrected to a different pet, which now belongs to them.
  -- Mirrors the read paths' @confirmed = 1@ filter.
  rows <-
    query
      c
      ( obsSelect
          <> " WHERE id IN \
             \(SELECT obs_id FROM subject_identity WHERE pet_id = ? AND confirmed = 1 \
             \ UNION SELECT obs_id FROM keepsakes WHERE pet_id = ? \
             \        AND obs_id NOT IN (SELECT obs_id FROM subject_identity WHERE pet_id <> ? AND confirmed = 1))"
      )
      (pid, pid, pid)
  -- Delete by id off the raw row, so a moment whose perception fails to decode is still
  -- removed. The decode only exists to hand the caller live rows for proof cleanup.
  let oids = map obsRowId rows
      (bad, deleted) = partitionEithers (map rowToObs rows)
  mapM_ (traceWith tracer . UnreadableRow "purge" . T.pack) bad
  -- The moment's keepsake, facts, and identity rows all cascade from the FK.
  forM_ oids $ \oid ->
    execute c "DELETE FROM observations WHERE id = ?" (Only oid)
  -- Anything else still tied to the pet by id: its keepsakes of other moments, its cached
  -- summary, and any stray override rows.
  execute c "DELETE FROM subject_identity WHERE pet_id = ?" (Only pid)
  execute c "DELETE FROM keepsakes WHERE pet_id = ?" (Only pid)
  execute c "DELETE FROM pet_summaries WHERE pet_id = ?" (Only pid)
  -- A hard purge is a true erase, so the pet's durable stats rollup goes too; it has no FK
  -- to the observations that cascaded above. Archiving a pet, the default soft-remove,
  -- never reaches here, so a retired pet's history survives.
  execute c "DELETE FROM daily_pet_stats WHERE pet_id = ?" (Only pid)
  pure deleted

-- --------------------------------------------------------------------------- --
-- State (watermark) and reports
-- --------------------------------------------------------------------------- --

getState :: Handle -> Text -> IO (Maybe Text)
getState h key = withConn h $ \c -> do
  rows <- query c "SELECT v FROM state WHERE k = ?" (Only key)
  pure (fromOnly <$> listToMaybe rows)

setState :: Handle -> Text -> Text -> IO ()
setState h key val = withConn h $ \c ->
  execute
    c
    "INSERT INTO state (k, v) VALUES (?, ?) \
    \ON CONFLICT(k) DO UPDATE SET v = excluded.v"
    (key, val)

-- | The ingest watermark: the POSIX-second @start_time@ the event sweep has advanced past.
-- 'Nothing' before the first sweep has ever run, which is a fresh install rather than a
-- stalled one, so the two stay distinguishable.
--
-- Typed here rather than left to each caller. The pipeline writes it and the web layer reads
-- it to answer "is this day caught up", and a key string or a parse that drifted between the
-- two would read as "never ingested" and quietly answer yes to everything.
getIngestWatermark :: Handle -> IO (Maybe Double)
getIngestWatermark h = (>>= readMaybe . T.unpack) <$> getState h ingestWatermarkKey

setIngestWatermark :: Handle -> Double -> IO ()
setIngestWatermark h = setState h ingestWatermarkKey . T.pack . show

ingestWatermarkKey :: Text
ingestWatermarkKey = "last_event_ts"

-- | Whether a past day's own event window has been swept to its end.
--
-- A day rebuild sweeps that one day straight from Frigate and deliberately leaves the global
-- watermark alone, so rebuilding an old day cannot rewind steady-state ingest. That makes the
-- watermark the wrong thing to ask "is this day finished": it can sit weeks behind a day that
-- is in fact complete. This is the per-day answer, set once a rebuild drains the day.
getDaySwept :: Handle -> Day -> IO Bool
getDaySwept h day = isJust <$> getState h (daySweptKey day)

setDaySwept :: Handle -> Day -> IO ()
setDaySwept h day = setState h (daySweptKey day) "1"

daySweptKey :: Day -> Text
daySweptKey day = "day_swept_" <> T.pack (showGregorian day)

-- | Store the report for its (day, period), refreshing the narrative in place if one
-- already exists. The unique index makes this an upsert rather than a duplicate row.
insertReport :: Handle -> Report -> IO ()
insertReport h r = withConn h $ \c ->
  execute
    c
    "INSERT INTO reports (ts, day, period, narrative) VALUES (?, ?, ?, ?) \
    \ON CONFLICT(day, period) DO UPDATE SET narrative = excluded.narrative, ts = excluded.ts"
    ( posixOf (reportAt r)
    , T.pack (show (reportDay r))
    , periodText (period r)
    , narrative r
    )

-- | Whether a report already exists for this (day, period), so a push notification goes out
-- only for a genuinely new report and not a refresh.
reportExists :: Handle -> Day -> Period -> IO Bool
reportExists h d per = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT 1 FROM reports WHERE day = ? AND period = ? LIMIT 1"
      (T.pack (show d), periodText per) ::
      IO [Only Int]
  pure (not (null rows))

-- | The most recently written report narrative for a day, whichever period wrote last, for
-- the day view's report card. Just the text; nothing at read time needs the scalar meta.
latestReport :: Handle -> Day -> IO (Maybe Text)
latestReport h d = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT narrative FROM reports WHERE day = ? ORDER BY ts DESC LIMIT 1"
      (Only (T.pack (show d)))
  pure (fromOnly <$> listToMaybe rows)

-- | The start time of the most recent stored EVENT observation per @(camera, label)@ within
-- @[lo, hi)@, as @(camera, label, ts)@ rows.
--
-- The ingest fold seeds its cooldown map from this, so a burst split across two pages or two
-- passes still honours the cooldown. A map that started empty each pass would let the first
-- event of the second page through even with a same-key event stored seconds earlier. Only
-- event rows carry a label, so sample rows are excluded.
recentEventStarts :: Handle -> Double -> Double -> IO [(Text, Text, Double)]
recentEventStarts h lo hi = withConn h $ \c ->
  query
    c
    "SELECT camera, label, MAX(ts) FROM observations \
    \WHERE source = 'event' AND label IS NOT NULL AND ts >= ? AND ts < ? \
    \GROUP BY camera, label"
    (lo, hi)

-- | Whether an observation already exists for this Frigate event id. Lets the ingest fold
-- advance past an event it has already stored without re-fetching the snapshot or re-running
-- vision, which is what makes a re-fetched window edge or a past-day rebuild cheap and
-- idempotent.
eventStored :: Handle -> Text -> IO Bool
eventStored h eid = withConn h $ \c -> do
  rows <-
    query c "SELECT 1 FROM observations WHERE event_id = ? LIMIT 1" (Only eid) ::
      IO [Only Int]
  pure (not (null rows))

-- --------------------------------------------------------------------------- --
-- Correction
-- --------------------------------------------------------------------------- --

-- | Rewrite an observation's stored perception by @f@ and mark it reviewed.
-- Returns 'False' if the id is unknown. Shared by owner corrections and edits.
updateObservationPerception :: Handle -> Int64 -> (Perception -> Perception) -> IO Bool
updateObservationPerception h oid f = do
  now <- getCurrentTime
  -- Read and write on one connection inside a transaction, so two overlapping corrections
  -- serialize instead of losing an update, and the UPDATE and re-projection commit
  -- together rather than being seen half-applied.
  withConn h $ \c -> withImmediateTransaction c $ do
    m <- getObservationC c oid
    case m of
      Nothing -> pure False
      Just obs -> do
        let p' = f (perception obs)
        execute
          c
          "UPDATE observations SET perception = ?, reviewed = 1, reviewed_at = ?, confidence = ?, uncertain = ? WHERE id = ?"
          (TL.toStrict (encodeToLazyText p'), posixOf now, confOf p', boolToInt (isUncertain p'), oid)
        -- Re-project the facts. raw_perception stays untouched, so the model's original
        -- reading survives the correction as an audit trail.
        writeFacts c oid p'
        pure True

-- | Apply an owner correction. A species or person target rewrites the perception blob,
-- retargeting the animal appearances. A specific-pet or visiting target is an /individual/
-- attribution stored as an override, which leaves both the blob and @raw_perception@
-- species-level. A person target also clears any such override.
correctObservation :: Handle -> Int64 -> Correction -> IO Bool
correctObservation h oid corr = case corr of
  ToPet pid   -> writeOverride h oid (IdPet pid)
  ToVisiting  -> writeOverride h oid IdVisiting
  ToSpecies _ -> updateObservationPerception h oid (applyCorrection corr)
  ToPerson    -> clearOverride h oid *> updateObservationPerception h oid (applyCorrection corr)

-- | Persist an owner identity override for every animal appearance of an observation and
-- mark it reviewed. A person appearance is never given a pet or visitor identity. Returns
-- 'False' for an unknown id.
--
-- This deliberately does NOT call 'writeFacts' or touch @perception@ and @raw_perception@.
-- Only the attribution changes, resolved at read time, which keeps the audit trail and the
-- model correction rate meaningful.
writeOverride :: Handle -> Int64 -> SubjectId -> IO Bool
writeOverride h oid sid = do
  now <- getCurrentTime
  withConn h $ \c -> withImmediateTransaction c $ do
    m <- getObservationC c oid
    case m of
      Nothing -> pure False
      Just obs -> do
        let animalSeqs = [s | (s, f) <- zip [0 ..] (factsOf (perception obs)), not (sfIsPerson f)]
        forM_ animalSeqs $ \s ->
          execute
            c
            "INSERT OR REPLACE INTO subject_identity \
            \(obs_id, seq, pet_id, visiting, source, confirmed, ts) \
            \VALUES (?, ?, ?, ?, 'owner', 1, ?)"
            (oid, s :: Int, petIdOf sid, visitingOf sid, posixOf now)
        execute c "UPDATE observations SET reviewed = 1, reviewed_at = ? WHERE id = ?" (posixOf now, oid)
        pure True
  where
    petIdOf (IdPet pid) = Just (petIdText pid)
    petIdOf IdVisiting  = Nothing
    visitingOf IdVisiting = 1 :: Int
    visitingOf (IdPet _)  = 0

-- | Drop all identity overrides for an observation.
clearOverride :: Handle -> Int64 -> IO ()
clearOverride h oid = withConn h $ \c ->
  execute c "DELETE FROM subject_identity WHERE obs_id = ?" (Only oid)

-- | Undo the owner's review or correction of a moment: restore the model's original reading
-- from @raw_perception@, drop any identity overrides, and mark it unreviewed so it can be
-- looked at afresh. With no stored original, just clear the overrides and unreview. All on
-- one connection in a transaction, as 'updateObservationPerception' does. 'False' for an
-- unknown id.
revertObservation :: Handle -> Int64 -> IO Bool
revertObservation h oid = withConn h $ \c -> withImmediateTransaction c $ do
  rows <- query c "SELECT raw_perception FROM observations WHERE id = ?" (Only oid) :: IO [Only (Maybe Text)]
  case rows of
    [] -> pure False
    (Only mraw : _) -> do
      execute c "DELETE FROM subject_identity WHERE obs_id = ?" (Only oid)
      case mraw of
        Just raw | Right p <- eitherDecodeStrict (encodeUtf8 raw) -> do
          execute
            c
            "UPDATE observations SET perception = ?, reviewed = 0, reviewed_at = NULL, confidence = ?, uncertain = ? WHERE id = ?"
            (raw, confOf p, boolToInt (isUncertain p), oid)
          writeFacts c oid p
        _ -> execute c "UPDATE observations SET reviewed = 0, reviewed_at = NULL WHERE id = ?" (Only oid)
      pure True

-- | Apply an owner field edit to an observation and mark it reviewed.
editObservation :: Handle -> Int64 -> SceneEdit -> IO Bool
editObservation h oid e = updateObservationPerception h oid (applyEdit e)

-- --------------------------------------------------------------------------- --
-- Keepsakes
-- --------------------------------------------------------------------------- --

-- | An observation the owner chose to keep.
data Keepsake = Keepsake
  { kId      :: Int64
  , kObsId   :: Int64
  , kPetId   :: Maybe Text
  , kCaption :: Maybe Text
  , kAt      :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Keepsake where
  toJSON = genericToJSON (prefixed 1)

-- | Save a moment as a keepsake, idempotently. A moment has at most one keepsake, enforced
-- by the schema's UNIQUE(obs_id) index.
--
-- A re-save happens when the Lightbox forgets it kept a moment after the owner navigates
-- away and back. It hits the conflict and DOES NOTHING, keeping the FIRST caption rather
-- than overwriting it. The existing row is then read back, so the handler returns a real
-- Keepsake with its actual id and caption whether this call created it or found it. Insert
-- and read run in one IMMEDIATE transaction, so the row selected is the one this call left.
insertKeepsake :: Handle -> Int64 -> Maybe Text -> Maybe Text -> UTCTime -> IO Keepsake
insertKeepsake h oid pid cap now = withConn h $ \c -> withImmediateTransaction c $ do
  execute
    c
    "INSERT INTO keepsakes (obs_id, pet_id, caption, ts) VALUES (?, ?, ?, ?) \
    \ON CONFLICT(obs_id) DO NOTHING"
    (oid, pid, cap, posixOf now)
  rows <-
    query
      c
      "SELECT id, obs_id, pet_id, caption, ts FROM keepsakes WHERE obs_id = ?"
      (Only oid)
  case rows of
    (r : _) -> pure (toKeepsake r)
    -- Unreachable. The INSERT either created the row or it already existed, so a row for
    -- this obs_id is always present by the time we read on the same connection in the same
    -- transaction. Fall back to the caller's values rather than failing.
    []      -> pure (Keepsake 0 oid pid cap now)

-- | All keepsakes, newest first, optionally filtered to one pet's.
listKeepsakes :: Handle -> Maybe Text -> IO [Keepsake]
listKeepsakes h mpid = withConn h $ \c -> do
  rows <- case mpid of
    Just pid ->
      query
        c
        "SELECT id, obs_id, pet_id, caption, ts FROM keepsakes \
        \WHERE pet_id = ? ORDER BY ts DESC"
        (Only pid)
    Nothing ->
      query_ c "SELECT id, obs_id, pet_id, caption, ts FROM keepsakes ORDER BY ts DESC"
  pure (map toKeepsake rows)

-- | The observation ids that have a keepsake. A kept moment's media belongs to pet-report,
-- never expires and survives GC, so the view layer reads this set to suppress the
-- clip-expiry countdown and mark the card. Keepsakes are few, so an unfiltered read is
-- cheap.
keptObsIds :: Handle -> IO [Int64]
keptObsIds h = withConn h $ \c ->
  map fromOnly <$> query_ c "SELECT DISTINCT obs_id FROM keepsakes"

-- | The @(camera, POSIX-second)@ stamps of kept periodic-sample moments. A sample's proof
-- frame is pet-report's own media, so 'pruneProof' skips these and a kept sample stays
-- durable.
keptSampleStamps :: Handle -> IO [(Text, Integer)]
keptSampleStamps h = withConn h $ \c -> do
  rows <-
    query_
      c
      "SELECT o.camera, o.ts FROM observations o \
      \JOIN keepsakes k ON k.obs_id = o.id WHERE o.source = 'sample'" ::
      IO [(Text, Double)]
  pure [(cam, round ts) | (cam, ts) <- rows]

-- | Decode a keepsakes row into a 'Keepsake', converting the stored POSIX seconds to
-- 'UTCTime'. Both 'listKeepsakes' and the read-back in 'insertKeepsake' go through here.
toKeepsake :: (Int64, Int64, Maybe Text, Maybe Text, Double) -> Keepsake
toKeepsake (i, o, p, cap, ts) =
  Keepsake i o p cap (fromPosix ts)

-- | Delete a keepsake by id, returning the observation id it referenced when there was one,
-- so the caller can free that moment's owned media on un-keep.
deleteKeepsake :: Handle -> Int64 -> IO (Maybe Int64)
deleteKeepsake h i = withConn h $ \c -> withImmediateTransaction c $ do
  rows <- query c "SELECT obs_id FROM keepsakes WHERE id = ?" (Only i) :: IO [Only Int64]
  execute c "DELETE FROM keepsakes WHERE id = ?" (Only i)
  pure (fromOnly <$> listToMaybe rows)

-- --------------------------------------------------------------------------- --
-- Per-pet weekly summaries (cached model output)
-- --------------------------------------------------------------------------- --

-- | The cached deterministic wellbeing verdict, the model-written one-line recap, and a few
-- stat pairs. The wellbeing line shown to the owner is derived from real signals, not
-- free text.
data PetSummary = PetSummary
  { sumKind  :: WellbeingKind
  , sumRecap :: Maybe Text
  , sumStats :: [(Text, Text)]
  }
  deriving stock (Eq, Show, Generic)

getPetSummary :: Handle -> Text -> IO (Maybe PetSummary)
getPetSummary h pid = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT wellbeing_kind, recap_line, stats \
      \FROM pet_summaries WHERE pet_id = ?"
      (Only pid)
  pure $ case rows of
    -- The wellbeing_kind column is a TEXT "good" or "watch". Parse it back into the sum
    -- here, tolerantly, since the cache is regenerable.
    ((k, rl, st) : _) -> Just (PetSummary (wellbeingKindFromText k) rl (decodeStats st))
    _                 -> Nothing
  where
    decodeStats t = fromMaybe [] (decodeStrict (encodeUtf8 t))

putPetSummary :: Handle -> Text -> UTCTime -> PetSummary -> IO ()
putPetSummary h pid now s = withConn h $ \c ->
  execute
    c
    "INSERT INTO pet_summaries \
    \(pet_id, wellbeing_kind, recap_line, stats, ts) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT(pet_id) DO UPDATE SET \
    \wellbeing_kind = excluded.wellbeing_kind, \
    \recap_line = excluded.recap_line, \
    \stats = excluded.stats, ts = excluded.ts"
    ( pid
    , wellbeingKindText (sumKind s)
    , sumRecap s
    , TL.toStrict (encodeToLazyText (sumStats s))
    , posixOf now
    )
