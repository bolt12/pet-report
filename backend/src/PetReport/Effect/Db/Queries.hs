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
  , allAttributionsForObs
  , countedAttributionsForObsIds
  , subjectStatsBetween
  , markReviewed
  , deleteObservation
  , deleteObservations
  , purgePetAndProfile
  , insertReport
  , reportExists
  , latestReport
  , eventStored
  , recentEventStarts
  , correctObservation
  , addObservationSighting
  , removeObservationSighting
  , revertObservation
  , unreviewObservation
  , editObservation
  , Settle (..)
  , AddOutcome (..)
  , maxSightingsPerMoment
  , reprojectAll
  , repairNamedSpecies
  , correctionStats
  , tsRange
  , obsEventId
  , setTranscript
  , transcriptsFor
  , schemaVersion
    -- Low-level row helpers shared with "PetReport.Effect.Db.Browse".
  , ObsRow
  , rowToObs
  , obsRowId
  , obsRowTs
  ) where

import           Control.Monad                  (forM_, when)
import           Data.Aeson                     (eitherDecodeStrict)
import           Data.Aeson.Text                (encodeToLazyText)
import           Data.Either                    (partitionEithers)
import           Data.Int                       (Int64)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (listToMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (encodeUtf8)
import qualified Data.Text.Lazy                 as TL
import           Data.Time                      (Day, UTCTime, getCurrentTime)
import           Database.SQLite.Simple         (Connection, Only (..),
                                                 Query (..), changes,
                                                 execute, execute_, fromOnly,
                                                 lastInsertRowId, query, query_,
                                                 withImmediateTransaction,
                                                 (:.) (..))
import           Database.SQLite.Simple.FromRow (FromRow (..))
import           GHC.Generics                   (Generic)

import           PetReport.Domain.Observation   (FrigateMeta (..),
                                                 NewObservation (..),
                                                 Observation (..), Origin (..),
                                                 originMeta)
import           PetReport.Domain.Perception    (Appearance (..), Correction (..),
                                                 Perception, Perception (..),
                                                 SceneEdit, Who,
                                                 addSighting, applyCorrectionAt,
                                                 applyEditAt, hasSightingAt,
                                                 removeSightingAt,
                                                 sceneAppearances)
import           PetReport.Domain.Profile       (Attribution (..),
                                                 Certainty (..), Overrides,
                                                 Pet (..), Profile, Roster,
                                                 SubjectId (..), emptyProfile,
                                                 namedPet, resolvePetNames)
import           PetReport.Domain.Report        (Period (..), Report (..),
                                                 periodText)
import           PetReport.Domain.Stats         (PetStat (..), SubjectFact (..),
                                                 StoredStats (..),
                                                 SubjectKey (..), confidenceOf,
                                                 factsOf, isUncertain, needsLook)
import           PetReport.Domain.Types         (Camera (..), EventId (..),
                                                 ObsId (..), PetId (..),
                                                 Species (..), cameraText,
                                                 petIdText)
import           PetReport.Effect.Db.Facts      (boolToInt,
                                                 decodePerception, writeFacts)
import           PetReport.Effect.Db.Handle     (Handle (..), withConn)
import           PetReport.Effect.Db.Migrations (currentVersion)
import           PetReport.Effect.Db.Sql        (fromPosix, placeholders, posixOf)
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
          "UPDATE observations SET confidence = ?, needs_look = ? WHERE id = ?"
          (confidenceOf p, boolToInt (needsLook p), oid)
      Left _  -> pure ()
  pure (length rows)

-- | Fold every stored reading that named a household pet where a species belongs back onto
-- that pet's species, and report how many sightings moved. Idempotent, so it is safe to run
-- on every start; a roster with no pets has nothing to match and exits at once.
--
-- 'PetReport.Domain.Profile.resolvePetNames' explains why such a reading is wrong. This
-- is the half that cannot be fixed forward: rows written before the vision decode learned
-- the repair are already in the database, invisible to the pet facet and splitting the day's
-- per-pet totals in two.
--
-- @raw_perception@ is rewritten alongside @perception@, unlike an owner correction, which
-- deliberately leaves the model's original reading intact. This is not a correction: it is
-- the same decode repair 'PetReport.Domain.Perception.normalizeScene' has always applied
-- before storage, arriving late. Leaving the phantom species in @raw_perception@ would hand
-- it straight back the first time an owner pressed Undo, since 'revertObservation' restores
-- from there.
repairNamedSpecies :: Handle -> Roster -> IO Int
repairNamedSpecies _ [] = pure 0
repairNamedSpecies h roster = withConn h $ \c -> withImmediateTransaction c $ do
  rows <-
    query_ c "SELECT id, perception, raw_perception FROM observations" ::
      IO [(Int64, Text, Maybe Text)]
  sum <$> mapM (repairRow c) rows
  where
    repairRow c (oid, perc, mraw) = case decodePerception perc of
      Left _ -> pure 0
      Right p -> do
        let p' = repair p
            moved = changedSightings p p'
        when (moved > 0) $ do
          execute
            c
            "UPDATE observations SET perception = ?, confidence = ?, needs_look = ? WHERE id = ?"
            (reencode p', confidenceOf p', boolToInt (needsLook p'), oid)
          writeFacts c oid p'
        -- The two blobs move independently. An owner who already corrected the sighting left
        -- @perception@ saying "dog" while @raw_perception@ still says "Yuki", so gating the
        -- raw rewrite on the live blob having moved skipped exactly the case this function
        -- documents: 'revertObservation' restores from raw, and would have handed the
        -- phantom species straight back.
        forM_ (mraw >>= either (const Nothing) Just . decodePerception) $ \raw -> do
          let raw' = repair raw
          when (changedSightings raw raw' > 0) $
            execute c "UPDATE observations SET raw_perception = ? WHERE id = ?" (reencode raw', oid)
        -- Runs whether or not the blob moved. A name the repair just rescued is the whole
        -- point of rescuing it, but a name already sitting in the blob with no row against
        -- it is the same sighting in the same state, and gating this on the rewrite left
        -- that one unreachable. Only ever ADDS a row: replacing would overwrite an owner's
        -- correction with the guess it was made to fix.
        writeModelIdentities Ignore c roster oid p'
        pure moved
    repair (Seen sc) = Seen (resolvePetNames roster sc)
    repair p         = p
    reencode = TL.toStrict . encodeToLazyText
    -- Count the sightings that actually moved, so the log line says how much of the history
    -- this touched rather than how many rows it walked.
    changedSightings a b =
      length [() | (x, y) <- zip (map who (sceneAppearances a)) (map who (sceneAppearances b)), x /= y]

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
insertObservation :: Handle -> Roster -> NewObservation -> IO ()
insertObservation h roster obs = withConn h $ \c -> withImmediateTransaction c $ do
  let perc = TL.toStrict (encodeToLazyText (noPerception obs))
      -- The Frigate meta an event carries, Nothing for a sample. Every stored column
      -- projects off this one Maybe, so "a sample has no meta" is stated once rather than
      -- re-cased per column.
      m = originMeta (noOrigin obs)
  execute
    c
    "INSERT OR IGNORE INTO observations \
    \(ts, camera, source, event_id, label, score, perception, reviewed, confidence, raw_perception, needs_look, has_clip, has_snapshot) \
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
      , confidenceOf (noPerception obs)
      , perc
      )
        :. ( boolToInt (needsLook (noPerception obs))
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
    writeModelIdentities Replace c roster oid (noPerception obs)

-- | Record which household pet the model said each subject was, resolved against the roster.
--
-- This is the answer to "which of my two dogs is that", and the only one available: species
-- alone cannot separate them, and the owner naming every sighting by hand is the work the
-- feature exists to avoid. The claim lives in the same table an owner correction does, so
-- one resolution path serves both, and a later correction simply overwrites the row.
--
-- @confirmed@ carries the confidence verdict rather than a threshold in SQL: a naming the
-- model was sure of counts everywhere immediately, and an unsure one counts nowhere but is
-- kept, because the moment is already in the review backlog and the guess is what makes
-- settling it one tap. A name no pet answers to is dropped.
-- | What an insert does about a row that is already there. A closed pair rather than the SQL
-- word itself, so the statement is still built from this module's own vocabulary and never
-- from a fragment handed in by a caller.
data OnConflict
  = Replace
  -- ^ The reading is being stored now, so its naming is the current one.
  | Ignore
  -- ^ Yield to whatever is already recorded. The backfill runs over moments an owner may
  -- long since have corrected by hand, and replacing those would overwrite the correction
  -- with the guess it was made to fix.
  deriving stock (Eq, Show)

conflictSql :: OnConflict -> Query
conflictSql Replace = "REPLACE"
conflictSql Ignore  = "IGNORE"

writeModelIdentities :: OnConflict -> Connection -> Roster -> Int64 -> Perception -> IO ()
writeModelIdentities conflict c roster oid p = do
  now <- getCurrentTime
  let sure = boolToInt (not (isUncertain p))
      named =
        [ (seq', petId pr)
        | (seq', ap) <- zip [0 :: Int ..] (sceneAppearances p)
        , Just pr <- [namedPet roster ap]
        ]
      stmt =
        "INSERT OR " <> conflictSql conflict <> " INTO subject_identity \
        \(obs_id, seq, pet_id, visiting, source, confirmed, ts) \
        \VALUES (?, ?, ?, 0, ?, ?, ?)"
  forM_ named $ \(seq', pid) ->
    execute c stmt (oid, seq', petIdText pid, modelSource, sure, posixOf now)

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
subjectStatsBetween :: Handle -> UTCTime -> UTCTime -> IO StoredStats
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
    StoredStats $
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

-- | Identity attributions for observations in @[lo, hi)@, keyed by @(obs_id, seq)@ for the
-- blob read path.
--
-- @confirmed = 1@ is what "trusted enough to count" means, and it is decided at the write:
-- an owner correction always earns it, a model naming earns it only when the model was sure.
-- Storing the answer keeps the confidence threshold out of SQL, the same way migration 2
-- keeps it out of the backlog predicate, and it is why every counting query downstream needs
-- no notion of where an attribution came from.
overridesBetween :: Handle -> UTCTime -> UTCTime -> IO Overrides
overridesBetween h lo hi = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT si.obs_id, si.seq, si.pet_id, si.visiting, si.source, si.confirmed, o.reviewed \
      \FROM subject_identity si JOIN observations o ON o.id = si.obs_id \
      \WHERE si.confirmed = 1 AND o.ts >= ? AND o.ts < ?"
      (posixOf lo, posixOf hi)
  pure (Map.fromList (map toOverride rows))

-- | Every identity attribution for a single observation, INCLUDING the ones held back from
-- the totals. For the single-moment read behind the review panel, which is the one screen whose
-- job is to settle an unsure naming: showing the model's guess there is what turns
-- confirming it into one tap instead of a choice from the whole roster.
--
-- Every other reader counts, so every other reader keeps the @confirmed = 1@ filter.
allAttributionsForObs :: Handle -> Int64 -> IO Overrides
allAttributionsForObs h oid = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT si.obs_id, si.seq, si.pet_id, si.visiting, si.source, si.confirmed, o.reviewed \
      \FROM subject_identity si JOIN observations o ON o.id = si.obs_id \
      \WHERE si.obs_id = ?"
      (Only oid)
  pure (Map.fromList (map toOverride rows))

-- | The COUNTED identity attributions for a SET of observations in ONE query. The id-set
-- companion to 'allAttributionsForObs', for enriching moments that share no time window, as
-- keepsakes do. An empty id list short-circuits, since an empty @IN ()@ is not valid SQL.
countedAttributionsForObsIds :: Handle -> [Int64] -> IO Overrides
countedAttributionsForObsIds _ [] = pure Map.empty
countedAttributionsForObsIds h oids = withConn h $ \c -> do
  rows <-
    query
      c
      ( "SELECT si.obs_id, si.seq, si.pet_id, si.visiting, si.source, si.confirmed, o.reviewed \
        \FROM subject_identity si JOIN observations o ON o.id = si.obs_id \
        \WHERE si.confirmed = 1 AND si.obs_id IN "
          <> inPlaceholders (length oids)
      )
      oids
  pure (Map.fromList (map toOverride rows))

-- | 'placeholders' lifted to 'Query'. Callers guard @n >= 1@, so this never renders an
-- empty @()@.
inPlaceholders :: Int -> Query
inPlaceholders = Query . placeholders

-- | The @subject_identity.source@ value for an attribution the vision model made rather than
-- the owner. Named once, since the write and the read have to agree on the word.
modelSource :: Text
modelSource = "model"

-- | Decode a @subject_identity@ row into an attribution (a specific pet, or a visiting
-- animal) together with how settled it is. Shared by the windowed and single-observation
-- reads.
--
-- Anything not written by the model counts as the owner's, so a source this does not
-- recognise is treated as the stronger claim rather than quietly downgraded.
toOverride :: (Int64, Int, Maybe Text, Int, Text, Int, Int) -> ((Int64, Int), Attribution)
toOverride (oid, s, mpid, vis, src, confirmed, reviewed) =
  let sid = case mpid of
        Just p | vis == (0 :: Int) -> IdPet (PetId p)
        _                          -> IdVisiting
      -- A model naming reads as a guess until the owner has agreed with the moment carrying
      -- it. Deriving that here rather than promoting the row when they press "That's right"
      -- is what keeps both directions honest: a write had nothing to undo it by, so
      -- unreviewing left the guess laundered into the owner's word, and reviewing a moment
      -- whose card never showed the name adopted it anyway.
      --
      -- @confirmed@ is the other half. A naming the model was unsure of never reaches a
      -- list card, so a verdict on the moment as a whole cannot be a verdict on it; only
      -- naming that sighting outright settles it.
      settled = src /= modelSource || (reviewed /= 0 && confirmed /= 0)
   in ((oid, s), Attribution sid (if settled then Confirmed else ByModel))

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
      -- Only the verdict is written. What it means for a name the model guessed at is read
      -- back out of it by 'toOverride', so taking the verdict back takes the meaning with
      -- it; a promotion written here would have had nothing to undo it by.
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
-- Reports
-- --------------------------------------------------------------------------- --

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

-- | Whether a write also settles the moment.
--
-- These are two different things an owner does, and conflating them is what made the review
-- flow unusable. Saying who is in a frame is a change to the READING; saying "that's right"
-- is a verdict on it. While every subject edit carried 'Confirm', the first one closed the
-- moment: the editor is only offered on an unsettled moment, so it vanished mid-edit, and
-- the only way back was Undo, which restores the model's reading and so discards the very
-- change that closed it. Editing a frame holding more than one subject was impossible.
data Settle
  = Confirm
  -- ^ The owner's verdict: this reading is right. Leaves the review backlog.
  | LeaveOpen
  -- ^ A change to the reading, with the verdict still to come. Stays in the backlog, so the
  -- owner keeps editing and settles it when they are done.
  deriving stock (Eq, Show)

-- | How many sightings one moment may hold. Not a domain truth, a runaway guard: every
-- sighting past the first is a deliberate tap, and each one counts again in the day's
-- per-pet totals, so an accidental repeat is a number the owner cannot explain. Far above
-- any real frame.
maxSightingsPerMoment :: Int
maxSightingsPerMoment = 16

-- | What became of a request to add a sighting, distinguished so the handler can answer
-- "there is nothing here to add to" and "this frame already holds too many" differently
-- rather than rendering both as a bare 404.
data AddOutcome
  = Added
  | NoSightingsHere
  -- ^ Unknown moment, or a sound, which has no sightings.
  | TooManySightings
  deriving stock (Eq, Show)

-- | Rewrite an observation's stored perception by @f@, settling it or not per @settle@, but
-- only when the stored perception satisfies @ok@. Shared by owner corrections and edits.
--
-- The guard runs on the row already read inside the transaction, so a sighting-range check
-- cannot race a concurrent edit that shrinks the scene. 'False' when the id is unknown OR
-- the guard rejects, which the handlers both render as a 404.
updateObservationPerceptionIf ::
  Handle -> Settle -> Int64 -> (Perception -> Bool) -> (Perception -> Perception) -> IO Bool
updateObservationPerceptionIf h settle oid ok f = do
  now <- getCurrentTime
  -- Read and write on one connection inside a transaction, so two overlapping corrections
  -- serialize instead of losing an update, and the UPDATE and re-projection commit
  -- together rather than being seen half-applied.
  withConn h $ \c -> withImmediateTransaction c $ do
    m <- getObservationC c oid
    case m of
      Nothing -> pure False
      Just obs | not (ok (perception obs)) -> pure False
      Just obs -> do
        let p' = f (perception obs)
        execute
          c
          "UPDATE observations SET perception = ?, confidence = ?, needs_look = ? WHERE id = ?"
          (TL.toStrict (encodeToLazyText p'), confidenceOf p', boolToInt (needsLook p'), oid)
        -- The verdict is its own statement, in the same transaction, and character for
        -- character the one 'markReviewed' runs. Splicing it into the UPDATE above meant
        -- hand-building the parameter list, which is the one place in this module where a
        -- placeholder and its argument could drift apart in silence.
        when (settle == Confirm) $
          execute c "UPDATE observations SET reviewed = 1, reviewed_at = ? WHERE id = ?" (posixOf now, oid)
        -- Re-project the facts. raw_perception stays untouched, so the model's original
        -- reading survives the correction as an audit trail.
        writeFacts c oid p'
        pure True

-- | Apply an owner correction to ONE sighting. A species or person target rewrites that
-- sighting inside the perception blob. A specific-pet or visiting target is an /individual/
-- attribution stored as an override, which leaves both the blob and @raw_perception@
-- species-level. A person target also drops that sighting's override, since a person holds
-- no pet identity.
--
-- 'False' for an unknown moment or a sighting index the scene does not hold. Addressing one
-- sighting is what lets a two-cat household name each cat in a frame that holds both; the
-- earlier observation-scoped version wrote the same identity onto every animal in the
-- moment, so correcting the cat also relabelled the dog beside it.
correctObservation :: Handle -> Int64 -> Int -> Correction -> IO Bool
correctObservation h oid ix corr = case corr of
  -- Settle the species in the blob FIRST, then write the identity. The order matters twice
  -- over: 'writeOverride' refuses a person sighting, so a mis-read person could not be
  -- named at all until the reading was fixed; and leaving a mis-detected species behind
  -- meant the projection still said "dog" for a sighting the owner had called a cat, so a
  -- species filter missed a moment the pet filter returned.
  ToPet pid _ -> do
    okBlob <- updateSighting h LeaveOpen oid ix (applyCorrectionAt ix corr)
    if okBlob then writeOverride h oid ix (IdPet pid) else pure False
  ToVisiting -> writeOverride h oid ix IdVisiting
  ToSpecies _ -> updateSighting h LeaveOpen oid ix (applyCorrectionAt ix corr)
  ToPerson ->
    clearOverrideAt h oid ix *> updateSighting h LeaveOpen oid ix (applyCorrectionAt ix corr)

-- | Rewrite a perception through @f@, but only when the addressed sighting exists. Keeps
-- the range check and the write in one transaction, so a concurrent edit cannot shrink the
-- scene between the two. The caller says whether the write is also the owner's verdict.
updateSighting :: Handle -> Settle -> Int64 -> Int -> (Perception -> Perception) -> IO Bool
updateSighting h settle oid ix = updateObservationPerceptionIf h settle oid (hasSightingAt ix)

-- | Persist an owner identity override for ONE animal sighting. A person sighting is never
-- given a pet or visitor identity, and addressing one returns 'False' rather than writing a
-- row that 'identifyWith' would ignore. 'False' also for an unknown moment or an
-- out-of-range index.
--
-- Naming a subject does not settle the moment; see 'Settle'.
--
-- This deliberately does NOT call 'writeFacts' or touch @perception@ and @raw_perception@.
-- Only the attribution changes, resolved at read time, which keeps the audit trail and the
-- model correction rate meaningful.
writeOverride :: Handle -> Int64 -> Int -> SubjectId -> IO Bool
writeOverride h oid ix sid = do
  now <- getCurrentTime
  withConn h $ \c -> withImmediateTransaction c $ do
    m <- getObservationC c oid
    case m of
      Nothing -> pure False
      Just obs -> case drop ix (factsOf (perception obs)) of
        (f : _) | ix >= 0 && not (sfIsPerson f) -> do
          execute
            c
            "INSERT OR REPLACE INTO subject_identity \
            \(obs_id, seq, pet_id, visiting, source, confirmed, ts) \
            \VALUES (?, ?, ?, ?, 'owner', 1, ?)"
            (oid, ix, petIdOf sid, visitingOf sid, posixOf now)
          pure True
        _ -> pure False
  where
    petIdOf (IdPet pid) = Just (petIdText pid)
    petIdOf IdVisiting  = Nothing
    visitingOf IdVisiting = 1 :: Int
    visitingOf (IdPet _)  = 0

-- | Add a sighting the model missed, as a neutral appearance of @w@. Appending never
-- disturbs an existing index, so no override needs remapping, and it does not settle the
-- moment: saying who else was there is a change to the reading, not a verdict on it.
--
-- The cap is checked on the row read inside the transaction, so two overlapping adds cannot
-- both see room for the last one.
addObservationSighting :: Handle -> Int64 -> Who -> IO AddOutcome
addObservationSighting h oid w = do
  added <- updateObservationPerceptionIf h LeaveOpen oid roomToAdd (addSighting w)
  if added
    then pure Added
    else do
      -- Separate the two ways the guard rejects, so a full frame does not read as a missing
      -- one. A second read is enough: nothing removes sightings behind our back except the
      -- owner, and either answer is then the truth a moment later.
      m <- getObservation h oid
      pure $ case perception <$> m of
        Just p | isScene p -> TooManySightings
        _                  -> NoSightingsHere
  where
    roomToAdd p = isScene p && length (sceneAppearances p) < maxSightingsPerMoment

-- | Drop a sighting the model invented. Does not settle the moment: saying someone was not
-- there is a change to the reading, not a verdict on it.
--
-- Removal is the one structural edit that RENUMBERS: every sighting after @ix@ shifts down
-- one. The identity overrides are keyed by that position, so they are remapped in the same
-- transaction as the blob rewrite. Without it, deleting the first of two subjects would
-- leave the second wearing the first's identity.
--
-- 'False' for an unknown moment or an index the scene does not hold.
removeObservationSighting :: Handle -> Int64 -> Int -> IO Bool
removeObservationSighting h oid ix =
  withConn h $ \c -> withImmediateTransaction c $ do
    m <- getObservationC c oid
    case m of
      Nothing -> pure False
      Just obs | not (hasSightingAt ix (perception obs)) -> pure False
      Just obs -> do
        let p' = removeSightingAt ix (perception obs)
        execute
          c
          "UPDATE observations SET perception = ?, confidence = ?, needs_look = ? WHERE id = ?"
          (TL.toStrict (encodeToLazyText p'), confidenceOf p', boolToInt (needsLook p'), oid)
        -- Remap the overrides to match the new numbering: the removed one goes, and every
        -- later one moves down. Done as delete-then-shift so the shift cannot collide with
        -- the row it is about to overwrite.
        execute c "DELETE FROM subject_identity WHERE obs_id = ? AND seq = ?" (oid, ix)
        execute
          c
          "UPDATE subject_identity SET seq = seq - 1 WHERE obs_id = ? AND seq > ?"
          (oid, ix)
        writeFacts c oid p'
        pure True

-- | Whether a perception is a scene at all, so the add path can refuse a sound.
isScene :: Perception -> Bool
isScene p = case p of
  Seen _  -> True
  Heard _ -> False

-- | Drop the identity override on one sighting, leaving its neighbours in the same moment
-- alone.
clearOverrideAt :: Handle -> Int64 -> Int -> IO ()
clearOverrideAt h oid ix = withConn h $ \c ->
  execute c "DELETE FROM subject_identity WHERE obs_id = ? AND seq = ?" (oid, ix)

-- | Take back the owner's verdict, leaving everything they said about the moment in place:
-- the reading they edited, and every identity they named. The moment returns to the backlog
-- if the model's own signals still put it there.
--
-- This is the undo of 'markReviewed', and the other half of 'revertObservation', which
-- throws the owner's work away as well. Keeping them apart is what lets "I confirmed that by
-- mistake" cost nothing: while the only undo restored the model's reading, taking back a
-- verdict also discarded the corrections that earned it.
--
-- 'False' for an unknown id.
unreviewObservation :: Handle -> Int64 -> IO Bool
unreviewObservation h oid = withConn h $ \c -> do
  execute c "UPDATE observations SET reviewed = 0, reviewed_at = NULL WHERE id = ?" (Only oid)
  (> 0) <$> changes c

-- | Undo the owner's review or correction of a moment: restore the model's original reading
-- from @raw_perception@, drop any identity overrides, and mark it unreviewed so it can be
-- looked at afresh. With no stored original, just clear the overrides and unreview. All on
-- one connection in a transaction, as 'updateObservationPerception' does. 'False' for an
-- unknown id.
revertObservation :: Handle -> Roster -> Int64 -> IO Bool
revertObservation h roster oid = withConn h $ \c -> withImmediateTransaction c $ do
  rows <- query c "SELECT raw_perception FROM observations WHERE id = ?" (Only oid) :: IO [Only (Maybe Text)]
  case rows of
    [] -> pure False
    (Only mraw : _) -> do
      execute c "DELETE FROM subject_identity WHERE obs_id = ?" (Only oid)
      case mraw of
        Just raw | Right stored <- eitherDecodeStrict (encodeUtf8 raw) -> do
          -- Through the same repair the decode applies. "What I first saw" means the model's
          -- reading in this app's vocabulary, not a blob from before the app could read it,
          -- so an old raw naming a pet where a species belongs is settled on the way back
          -- rather than restored as the phantom species it was stored as.
          let p = case stored of
                Seen sc -> Seen (resolvePetNames roster sc)
                other   -> other
          execute
            c
            "UPDATE observations SET perception = ?, reviewed = 0, reviewed_at = NULL, confidence = ?, needs_look = ? WHERE id = ?"
            (TL.toStrict (encodeToLazyText p), confidenceOf p, boolToInt (needsLook p), oid)
          writeFacts c oid p
          -- Back to the model's reading means ALL of it. Its identification was part of what
          -- it first saw, so clearing the owner's names and then leaving the sighting
          -- anonymous would land somewhere the model never reported.
          writeModelIdentities Replace c roster oid p
        _ -> execute c "UPDATE observations SET reviewed = 0, reviewed_at = NULL WHERE id = ?" (Only oid)
      pure True

-- | Apply an owner field edit to an observation and settle it. Unlike a subject change,
-- this one IS a verdict: the panel it comes from is captioned "Save & confirm", and saving
-- it is the owner saying the whole reading now says what they mean.
editObservation :: Handle -> Int64 -> Int -> SceneEdit -> IO Bool
editObservation h oid ix e = updateSighting h Confirm oid ix (applyEditAt ix e)
