-- | Small SQL primitives shared across the persistence submodules: the @UTCTime@ to REAL
-- POSIX-seconds storage decision, and an IN-clause placeholder builder, since sqlite-simple
-- has no @In@ helper. Depends on nothing else in the Db family, so every submodule can
-- import it without a cycle.
module PetReport.Effect.Db.Sql
  ( posixOf
  , fromPosix
  , placeholders
  ) where

import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)

-- | Encode a time as the POSIX seconds (a REAL) the observation columns store.
posixOf :: UTCTime -> Double
posixOf = realToFrac . utcTimeToPOSIXSeconds

-- | Decode the stored POSIX seconds back to a time. The inverse of 'posixOf'.
fromPosix :: Double -> UTCTime
fromPosix = posixSecondsToUTCTime . realToFrac

-- | A parenthesised @(?, ?, ...)@ list of @n@ bind placeholders, for an IN clause.
placeholders :: Int -> Text
placeholders n = "(" <> T.intercalate "," (replicate n "?") <> ")"
