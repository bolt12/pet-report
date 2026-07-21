-- | The @days@ resource handler: one local day as enriched moments, the cached
-- narrative, coarse presence, and per-pet stats.
module PetReport.Web.Days
  ( dayH
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Servant                (Handler)

import           PetReport.App           (App (..))
import           PetReport.Domain.Window (Window (..), localDayOf, parseWindow,
                                          recognizedArg)
import qualified PetReport.Effect.Db     as Db
import           PetReport.Error         (badInput)
import           PetReport.Web.Common    (dayStatsFor, dayViews, nowTzProfile)
import           PetReport.Web.Types     (DayResponse (..))

-- | One local day: enriched moments, the cached narrative, coarse presence, and per-pet
-- stats.
dayH :: App -> Text -> Handler DayResponse
dayH app d
  -- Reject a day the window parser cannot resolve, rather than silently returning today's
  -- data. 'parseWindow' falls back to today for an unrecognized argument.
  | not (recognizedArg d) = badInput "invalid day"
  | otherwise = liftIO $ do
      (now, tz, prof) <- nowTzProfile app
      let w = parseWindow tz now (Just d) (Just d)
      (views, pres) <- dayViews app now prof w
      narr <- Db.latestReport (appDb app) (localDayOf tz (winFrom w))
      stats <- dayStatsFor app tz prof w
      pure (DayResponse d views narr pres stats)
