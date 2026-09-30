-- | Per-day, per-subject aggregation over a window of days, oldest first, for the trends
-- panel. Days are local via 'TZ', and each day runs the same pure 'presence' aggregation
-- the rest of the app uses.
module PetReport.Domain.Trends
  ( DayTrend (..)
  , trends
  ) where

import qualified Data.Map.Strict              as Map
import           Data.Time                    (Day, UTCTime, addDays)
import           Data.Time.Zones              (TZ)
import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.Profile     (Overrides, Roster)
import           PetReport.Domain.Stats       (ResolvedStats, presence)
import           PetReport.Domain.Window      (localDayOf)

-- | One local day's slice of the window. 'dtObservations' counts observations, not
-- appearances, so it need not equal any subject's sightings.
data DayTrend = DayTrend
  { dtDay          :: Day
  , dtStats        :: ResolvedStats
  , dtObservations :: Int
  }
  deriving stock (Eq, Show)

-- | The last @n@ local days ending today, each with its per-subject stats.
trends :: TZ -> Overrides -> Roster -> Int -> UTCTime -> [Observation] -> [DayTrend]
trends tz ov roster n now obss =
  let m = max 1 n
      today = localDayOf tz now
      byDay =
        Map.fromListWith (++) [(localDayOf tz (at o), [o]) | o <- obss]
      mk d =
        let os = Map.findWithDefault [] d byDay
         in DayTrend
              { dtDay = d
              , dtStats = presence ov roster os
              , dtObservations = length os
              }
   in [mk (addDays (fromIntegral (i - (m - 1))) today) | i <- [0 .. m - 1]]
