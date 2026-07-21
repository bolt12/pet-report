-- | Pure enrichment that lifts a stored per-pet summary and a saved keepsake into their view
-- shapes, plus the wellbeing line. It sits outside @PetReport.Domain.*@ because it touches
-- the database row types, and the domain core stays free of the Effect layer. Everything
-- here is still pure, so the domain test suite can reach it.
module PetReport.View.Enrich
  ( wellbeingLine
  , mkRecap
  , mkKeepsake
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)

import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.PetReport (KeepsakeV (..), RecapV (..),
                                             StatPair (..), WellbeingKind (..))
import           PetReport.Domain.Profile   (CameraRoom, roomOf)
import           PetReport.Domain.View      (ObsMedia (..), mediaFor)
import qualified PetReport.Effect.Db        as Db

-- | Lift a stored per-pet summary into a recap view. 'Nothing' when the summary has no
-- recap line.
mkRecap :: Int -> Int -> Db.PetSummary -> Maybe RecapV
mkRecap weekSeen daysSeen s = case Db.sumRecap s of
  Just line -> Just (RecapV "this week" line (map fixStat (Db.sumStats s)))
  Nothing   -> Nothing
  where
    -- Keep the model's prose line, but pin the numeric "Seen" and "Days seen" to the live
    -- week totals, so they cannot contradict the 30-day tiles: day <= week <= month.
    fixStat (k, v)
      | k == "Seen"      = StatPair k (T.pack (show weekSeen) <> " this week")
      | k == "Days seen" = StatPair k (T.pack (show daysSeen) <> " of 7")
      | otherwise        = StatPair k v

-- | A short wellbeing line built from real signals: sightings this week, rest share,
-- favourite room. Hidden when the week is too thin to say anything meaningful.
wellbeingLine :: Text -> WellbeingKind -> Int -> Int -> Maybe Text -> Maybe Text
wellbeingLine name kind weekSeen restPct topSpot
  | weekSeen < 5 = Nothing
  | otherwise = Just (base <> spot <> close)
  where
    base = name <> " was seen " <> sh weekSeen <> " times this week, resting about " <> sh restPct <> "% of the time"
    spot = maybe "" (", most often in the " <>) topSpot
    close = if kind == Flagged then ". One moment is flagged for a look." else "."
    sh = T.pack . show

-- | Lift a saved keepsake plus its moment into the keepsake view, resolving the moment's
-- media against the given proof-retention window so the card gets a real thumbnail.
mkKeepsake :: Double -> [CameraRoom] -> UTCTime -> Db.Keepsake -> Observation -> KeepsakeV
mkKeepsake retainDays crs now k obs =
  let m = mediaFor retainDays now obs
   in KeepsakeV
        (Db.kId k)
        (Db.kObsId k)
        (Db.kCaption k)
        (roomOf crs (camera obs))
        (omImg m)
        (omKind m)
        (Db.kAt k)
