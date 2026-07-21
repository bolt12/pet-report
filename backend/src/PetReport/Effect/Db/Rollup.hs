-- | The durable per-local-day, per-subject stats rollup. It materialises the same
-- aggregation 'PetReport.Effect.Db.Queries.subjectStatsBetween' computes live, but persists
-- it per day, so a day's per-pet history outlives both the garbage collection of its raw
-- moments and a pet's retirement.
module PetReport.Effect.Db.Rollup
  ( materializeDay
  , dailyPetStats
  ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import           Database.SQLite.Simple     (Only (..), execute, field, query,
                                             withImmediateTransaction)
import           Database.SQLite.Simple.FromRow (FromRow (..))

import           PetReport.Domain.Stats     (PetStat (..), SubjectKey (..))
import           PetReport.Domain.Types     (PetId (..), Species (..))
import           PetReport.Effect.Db.Handle (Handle, withConn)
import           PetReport.Effect.Db.Sql    (posixOf)

-- | Re-materialise a local day's per-subject stats: delete the day's rows, then re-insert
-- them from the facts projection over the day's @[lo, hi)@ UTC window. Idempotent, so a
-- correction re-materialises cleanly. Mirrors 'subjectStatsBetween' exactly, same identity
-- join and same visitor exclusion, so the stored aggregate equals a live compute over the
-- same window.
materializeDay :: Handle -> Text -> UTCTime -> UTCTime -> IO ()
materializeDay h day lo hi = withConn h $ \c -> withImmediateTransaction c $ do
  execute c "DELETE FROM daily_pet_stats WHERE day = ?" (Only day)
  execute
    c
    "INSERT INTO daily_pet_stats \
    \(day, species, pet_id, sightings, rest, active, ate, drank, slept, played, groomed, eliminated, concern) \
    \SELECT ?, os.species, COALESCE(si.pet_id, ''), COUNT(*), SUM(os.rest), SUM(os.active), \
    \SUM(os.ate), SUM(os.drank), SUM(os.slept), SUM(os.played), SUM(os.groomed), \
    \SUM(os.eliminated), SUM(os.concern) \
    \FROM observation_subjects os JOIN observations o ON o.id = os.obs_id \
    \LEFT JOIN subject_identity si ON si.obs_id = os.obs_id AND si.seq = os.seq AND si.confirmed = 1 \
    \WHERE os.is_person = 0 AND os.species IS NOT NULL AND o.ts >= ? AND o.ts < ? \
    \AND COALESCE(si.visiting, 0) = 0 \
    \GROUP BY os.species, si.pet_id"
    (day, posixOf lo, posixOf hi)

-- | The stored per-subject stats for a local day, keyed exactly as 'subjectStatsBetween'
-- keys a live window: a specific pet by 'KPet', an unattributed sighting by 'KSpecies'. A
-- collected day therefore renders through the same code as a live one.
dailyPetStats :: Handle -> Text -> IO (Map SubjectKey PetStat)
dailyPetStats h day = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT species, pet_id, sightings, rest, active, ate, drank, slept, played, \
      \groomed, eliminated, concern FROM daily_pet_stats WHERE day = ?"
      (Only day)
  -- Merge with the summing 'PetStat' Semigroup, exactly as 'subjectStatsBetween' does. A
  -- pet confirmed across two DETECTED species, from a misdetection corrected to it, gives
  -- two rows under the same 'KPet', and their counts must sum rather than overwrite.
  pure $
    Map.fromListWith
      (<>)
      [ (key, ps)
      | DailyStatRow sp pid ps <- rows
      , let key = if T.null pid then KSpecies (Species sp) else KPet (PetId pid)
      ]

-- | A daily_pet_stats row: species, pet_id ('' for species-level), then the ten
-- 'PetStat' counters in column order.
data DailyStatRow = DailyStatRow !Text !Text !PetStat

instance FromRow DailyStatRow where
  fromRow =
    DailyStatRow
      <$> field
      <*> field
      <*> ( PetStat
              <$> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
          )
