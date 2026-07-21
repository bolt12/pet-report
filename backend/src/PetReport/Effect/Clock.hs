-- | The clock effect: current time and the local time zone, as a record of functions so a
-- test can inject a fixed clock. The zone comes from the tz database embedded in the @tz@
-- package, needing no runtime files, and is re-resolved on every read so a timezone change
-- in setup takes effect without a restart.
module PetReport.Effect.Clock
  ( Handle (..)
  , withHandle
  , resolveTz
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time              (UTCTime, getCurrentTime)
import           Data.Time.Zones        (TZ, utcTZ)
import           Data.Time.Zones.All    (tzByName)

data Handle = Handle
  { now      :: IO UTCTime
  , timeZone :: IO TZ
  -- ^ The effective local zone, re-resolved on each read from the profile's @timeZone@,
  -- falling back to the @PET_REPORT_TZ@ env default.
  }

-- | Build a real clock whose zone is resolved from @getTzName@ on every read. The getter is
-- an 'STM' read of shared config rather than arbitrary 'IO', so a setup change published to
-- that state is picked up on the next read.
withHandle :: STM Text -> (Handle -> IO a) -> IO a
withHandle getTzName k =
  k
    Handle
      { now = getCurrentTime
      , timeZone = resolveTz <$> atomically getTzName
      }

-- | Resolve an IANA zone name (e.g. @Europe/Lisbon@) to a 'TZ'. An unknown or empty name
-- falls back to UTC, so a bad value can never crash the clock.
resolveTz :: Text -> TZ
resolveTz name = fromMaybe utcTZ (tzByName (encodeUtf8 name))
