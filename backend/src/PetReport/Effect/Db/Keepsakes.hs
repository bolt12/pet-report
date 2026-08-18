-- | The two owner-facing side tables: keepsakes (moments the owner chose to keep, whose
-- media pet-report then owns so it outlives Frigate's pruning) and the per-pet weekly
-- summary cache.
--
-- Split out of "PetReport.Effect.Db.Queries", which had grown into nine unrelated
-- resources sharing one 944-line module. These two are the most self-contained of them:
-- nothing else reads their tables, and neither is on the ingest path.
module PetReport.Effect.Db.Keepsakes
  ( Keepsake (..)
  , insertKeepsake
  , listKeepsakes
  , keptObsIds
  , keptSampleStamps
  , deleteKeepsake
  , PetSummary (..)
  , getPetSummary
  , putPetSummary
  ) where

import           Data.Aeson                 (ToJSON (..), decodeStrict,
                                             genericToJSON)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Aeson.Text            (encodeToLazyText)
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time                  (UTCTime)
import           Database.SQLite.Simple     (Only (..), execute, fromOnly, query,
                                             query_, withImmediateTransaction)
import           GHC.Generics               (Generic)

import           PetReport.Domain.PetReport (WellbeingKind, wellbeingKindFromText,
                                             wellbeingKindText)
import           PetReport.Effect.Db.Handle (Handle, withConn)
import           PetReport.Effect.Db.Sql    (fromPosix, posixOf)
import           PetReport.Util             (prefixed)

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
