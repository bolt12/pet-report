-- | The @state@ key/value table and the typed accessors over it.
--
-- The table is a single @(k, v)@ pair store, so every value in it is a string that some
-- caller has to know how to read. That is the escape hatch pipeline progress kept reaching
-- for: three consecutive fixes to the catch-up logic each appended one more
-- @get@/@set@ pair here, which is why this is split out rather than left among the
-- observation queries.
--
-- The rule the pairs enforce is that NO caller outside this module names a key or parses a
-- value. 'getState' and 'setState' stay exported for a genuinely ad-hoc read, but every
-- durable piece of progress gets a typed pair, so a key can be renamed here without a
-- search across the pipeline.
module PetReport.Effect.Db.State
  ( getState
  , setState
  , getIngestWatermark
  , setIngestWatermark
  , getIngestDrained
  , setIngestDrained
  , getDaySwept
  , setDaySwept
  ) where

import           Data.Maybe                 (isJust, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (Day)
import           Data.Time.Calendar         (showGregorian)
import           Database.SQLite.Simple     (Only (..), execute, fromOnly, query)
import           Text.Read                  (readMaybe)

import           PetReport.Effect.Db.Handle (Handle, withConn)

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

-- | Whether the last scheduled ingest drained its window rather than parking on a backlog.
-- The current day's catch-up notice reads this instead of comparing the watermark to the
-- clock: between batches the watermark is legitimately behind "now", so the honest question
-- is whether work is outstanding, not whether the frontier sits at this exact instant. Absent
-- (a fresh install, or the first run after upgrading) reads as drained, so nothing cries wolf.
getIngestDrained :: Handle -> IO Bool
getIngestDrained h = maybe True (== "1") <$> getState h ingestDrainedKey

setIngestDrained :: Handle -> Bool -> IO ()
setIngestDrained h drained = setState h ingestDrainedKey (if drained then "1" else "0")

ingestDrainedKey :: Text
ingestDrainedKey = "ingest_drained"
