-- | Time windows and the loose window parser the Ask tools use ("today", "7d",
-- "2026-07-01"). Local-day boundaries go through a 'TZ' so they are DST-correct
-- for any date, unlike a fixed UTC offset. Pure: the current time and zone are
-- passed in (the 'Clock' effect supplies them).
module PetReport.Domain.Window
  ( Window (..)
  , parseWindow
  , recognizedArg
  , resolveDay
  , startOfLocalDay
  , localDayWindow
  , localDayOf
  , dayKeyText
  , localClock
  ) where

import           Data.Char          (isDigit)
import           Data.Maybe         (isJust)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time          (Day, LocalTime (..), UTCTime, addDays,
                                     defaultTimeLocale, formatTime, localDay,
                                     midnight, parseTimeM)
import           Data.Time.Calendar (showGregorian)
import           Data.Time.Zones    (TZ, localTimeToUTCTZ, utcToLocalTimeTZ)
import           Text.Read       (readMaybe)

data Window = Window
  { winFrom :: UTCTime
  , winTo   :: UTCTime
  }
  deriving stock (Eq, Show)

localDayOf :: TZ -> UTCTime -> Day
localDayOf tz t = localDay (utcToLocalTimeTZ tz t)

-- | A local day as the @YYYY-MM-DD@ key shared by the API and the durable stats rollup.
-- The batch writes a rollup under the same key a handler later reads it back by.
dayKeyText :: Day -> Text
dayKeyText = T.pack . showGregorian

-- | The local 'Day' a single-day argument resolves to, for a caller that wants the day
-- without the window. The refresh handler uses this to pick a job; the day and pets
-- handlers keep the whole window.
resolveDay :: TZ -> UTCTime -> Text -> Day
resolveDay tz now raw = localDayOf tz (winFrom (parseWindow tz now (Just raw) (Just raw)))

-- | Local wall-clock "HH:MM" of a UTC time in the given zone. The daily narrative and the
-- Ask prompt both render through here.
localClock :: TZ -> UTCTime -> Text
localClock tz t = T.pack (formatTime defaultTimeLocale "%H:%M" (utcToLocalTimeTZ tz t))

startOfLocalDay :: TZ -> Day -> UTCTime
startOfLocalDay tz d = localTimeToUTCTZ tz (LocalTime d midnight)

-- | The half-open UTC window @[start of the local day, min now (start of the next local
-- day))@. A past day is complete; the current day runs only up to @now@.
localDayWindow :: TZ -> UTCTime -> Day -> (UTCTime, UTCTime)
localDayWindow tz now d = (startOfLocalDay tz d, min now (startOfLocalDay tz (addDays 1 d)))

-- | Resolve a @(since, until)@ pair into a window. Defaults: @since@ is local
-- midnight today, @until@ is now. An inverted pair is swapped, never empty.
parseWindow :: TZ -> UTCTime -> Maybe Text -> Maybe Text -> Window
parseWindow tz now sinceArg untilArg =
  let today = localDayOf tz now
      a = resolve sinceArg (startOfLocalDay tz today) False
      b = resolve untilArg now True
   in if a <= b then Window a b else Window b a
  where
    resolve Nothing def _ = def
    resolve (Just raw) def isEnd =
      let s = T.toLower (T.strip raw)
       in if s == "today" || s == "now"
            then if isEnd then now else startOfLocalDay tz (localDayOf tz now)
            else case parseNd s of
              Just n -> startOfLocalDay tz (addDays (negate n) (localDayOf tz now))
              Nothing -> case parseDay s of
                Just d
                  | isEnd -> startOfLocalDay tz (addDays 1 d)
                  | otherwise -> startOfLocalDay tz d
                Nothing -> def

-- | Whether an argument is one 'parseWindow' recognizes ("today"/"now", "Nd", or
-- a date), rather than one it silently falls back to a default for. Lets a handler
-- reject a malformed day instead of quietly returning today's data.
recognizedArg :: Text -> Bool
recognizedArg raw =
  let s = T.toLower (T.strip raw)
   in s == "today" || s == "now" || isJust (parseNd s) || isJust (parseDay s)

-- | Parse "@N@d" / "@N@day" / "@N@days" into a number of days, bounded to @[1, 3650]@. An
-- out-of-range value (0, or a huge "9999...9d" off an Ask-tool argument) reads as
-- unrecognized rather than resolving to a nonsensical window. 'recognizedArg' goes through
-- here too, so validation and resolution agree on what counts as a valid "Nd".
parseNd :: Text -> Maybe Integer
parseNd s =
  let digits = T.takeWhile isDigit s
      rest = T.dropWhile isDigit s
   in if not (T.null digits) && rest `elem` ["d", "day", "days"]
        then case readMaybe (T.unpack digits) of
          Just n | n >= 1 && n <= 3650 -> Just n
          _                            -> Nothing
        else Nothing

parseDay :: Text -> Maybe Day
parseDay s = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack (T.take 10 s))
