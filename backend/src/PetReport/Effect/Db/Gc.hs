-- | Garbage collection of old un-kept moments. Once a day's raw moments are past the
-- retention window, the un-kept ones are caption and metadata with no proof behind them, so
-- they go. The day's durable stats rollup, materialised first, and any kept moments remain.
module PetReport.Effect.Db.Gc
  ( collectUnkept
  , hasUnkeptBetween
  , countCollectable
  ) where

import           Data.Maybe                 (listToMaybe)
import           Data.Time                  (UTCTime)
import           Database.SQLite.Simple     (Only (..), changes, execute,
                                             fromOnly, query,
                                             withImmediateTransaction)

import           PetReport.Effect.Db.Handle (Handle, withConn)
import           PetReport.Effect.Db.Sql    (posixOf)

-- | Delete the un-kept observations in @[lo, hi)@, meaning those NOT referenced by a
-- keepsake. Their facts and identity rows cascade away, the rollup having already captured
-- the day's stats. A kept moment keeps both its row and its owned media. Returns how many
-- rows were removed.
collectUnkept :: Handle -> UTCTime -> UTCTime -> IO Int
collectUnkept h lo hi = withConn h $ \c -> withImmediateTransaction c $ do
  execute
    c
    "DELETE FROM observations WHERE ts >= ? AND ts < ? \
    \AND id NOT IN (SELECT obs_id FROM keepsakes)"
    (posixOf lo, posixOf hi)
  changes c

-- | Whether @[lo, hi)@ still holds an un-kept observation. GC only processes a day while
-- this is true, because it materialises the rollup from the FULL day before collecting.
-- Once a day has been collected it is skipped, so its rollup is never rebuilt from the
-- kept-only remainder, which would shrink or wipe it.
hasUnkeptBetween :: Handle -> UTCTime -> UTCTime -> IO Bool
hasUnkeptBetween h lo hi = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT 1 FROM observations WHERE ts >= ? AND ts < ? \
      \AND id NOT IN (SELECT obs_id FROM keepsakes) LIMIT 1"
      (posixOf lo, posixOf hi) ::
      IO [Only Int]
  pure (not (null rows))

-- | How many un-kept observations are older than @cutoff@, i.e. how many moments GC
-- would remove at a given window. Backs the "this will delete N moments" warning
-- shown before the owner shortens the retention.
countCollectable :: Handle -> UTCTime -> IO Int
countCollectable h cutoff = withConn h $ \c -> do
  rows <-
    query
      c
      "SELECT COUNT(*) FROM observations WHERE ts < ? \
      \AND id NOT IN (SELECT obs_id FROM keepsakes)"
      (Only (posixOf cutoff)) ::
      IO [Only Int]
  pure (maybe 0 fromOnly (listToMaybe rows))
