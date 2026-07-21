-- | Handler helpers shared across the per-resource handler modules: the
-- now/timezone/profile preamble, the view-building reads with 'buildObsViews' at their
-- core, and the small response combinators for the 404 lookups and the media read shape.
module PetReport.Web.Common
  ( nowTzProfile
  , dayViews
  , buildObsViews
  , dayStatsFor
  , momentViewsByIds
  , obsViewOf
  , okOr404
  , foundOr404
  , validatedMediaH
  , orUnavailable
  ) where

import           Control.Concurrent.STM (readTVarIO)
import           Control.Exception      (fromException)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (listToMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime)
import           Data.Time.Zones        (TZ)
import           Servant                (Handler)

import           PetReport.App                (App (..), appRetention)
import           PetReport.Domain.Observation (Observation)
import           PetReport.Domain.Profile     (Overrides, Pet (..), Profile (..),
                                               petById)
import           PetReport.Domain.Stats       (PetStat (..), SubjectKey (..),
                                               homePresence)
import           PetReport.Domain.Types       (ObsId (..), petIdText, speciesText)
import           PetReport.Domain.View        (ObsView, RetentionMap, mkViews)
import           PetReport.Domain.Window      (Window (..), dayKeyText, localDayOf)
import qualified PetReport.Effect.Clock       as Clock
import qualified PetReport.Effect.Db          as Db
import           PetReport.Error              (AppError (..), badInput, notFound,
                                               throwAppError)
import qualified PetReport.Pipeline           as Pipeline
import           PetReport.Trace              (WebEvent (..), traceWith, webTracer)
import           PetReport.Util               (trySync)
import           PetReport.Web.Types          (DayStat (..), OkResp (..), PresenceV,
                                               presenceV)

-- | The preamble nearly every handler opens with: current time, timezone, profile.
nowTzProfile :: App -> IO (UTCTime, TZ, Profile)
nowTzProfile app = do
  now <- Clock.now (appClock app)
  prof <- Db.getProfile (appDb app)
  tz <- Clock.timeZone (appClock app)
  pure (now, tz, prof)

-- | Load a window's observations and build the two parts every day and range response
-- shares: the enriched 'ObsView's, with overrides applied and transcripts attached, and the
-- coarse presence. The narrative is NOT here, since a day has one and a range none, so each
-- handler adds its own. @now@ and @prof@ come in already read, so the caller can use them
-- too without a second load.
dayViews :: App -> UTCTime -> Profile -> Window -> IO ([ObsView], PresenceV)
dayViews app now prof w = do
  obss <- Db.observationsBetween (appDb app) (winFrom w) (winTo w)
  ov <- Db.overridesBetween (appDb app) (winFrom w) (winTo w)
  transcripts <- Db.transcriptsFor (appDb app) (winFrom w) (winTo w)
  views <- buildObsViews app now prof transcripts ov obss
  pure (views, presenceV (homePresence obss))

-- | The two extra inputs 'mkViews' needs beyond the pure domain data: the polled Frigate
-- retention, which drives the clip-expiry countdown, and the set of kept moments, which are
-- owned and so have none.
viewCtx :: App -> IO (RetentionMap, Set ObsId)
viewCtx app = do
  retention <- readTVarIO (appRetention app)
  kept <- Set.fromList . map ObsId <$> Db.keptObsIds (appDb app)
  pure (retention, kept)

-- | Enrich a batch of stored observations into 'ObsView's. This is how a moment becomes a
-- view, shared by the day, browse and by-id read paths. Each passes its own transcripts map,
-- or @mempty@ where transcripts are fetched on demand.
buildObsViews :: App -> UTCTime -> Profile -> Map.Map ObsId Text -> Overrides -> [Observation] -> IO [ObsView]
buildObsViews app now prof transcripts ov obss = do
  (retention, kept) <- viewCtx app
  pure (mkViews Pipeline.proofRetainDays retention kept now transcripts prof ov obss)

-- | Per-pet stats for the selected day. Labels resolve via the full roster, so an archived
-- pet keeps its name, and an unattributed sighting is labelled by species.
dayStatsFor :: App -> TZ -> Profile -> Window -> IO [DayStat]
dayStatsFor app tz prof w = do
  -- Prefer a live compute over the day's moments, which stays in step with a just-made
  -- correction, and fall back to the durable rollup only once the day has no moments left,
  -- meaning it has been garbage-collected. The rollup equals a live compute by
  -- construction, so the two agree while both are available.
  live <- Db.subjectStatsBetween (appDb app) (winFrom w) (winTo w)
  stats <-
    if Map.null live
      then Db.dailyPetStats (appDb app) (dayKeyText (localDayOf tz (winFrom w)))
      else pure live
  pure (map (toDayStat (pets prof)) (Map.toList stats))

toDayStat :: [Pet] -> (SubjectKey, PetStat) -> DayStat
toDayStat roster (key, ps) =
  DayStat label mpid species
    (psSightings ps) (psRest ps) (psActive ps) (psAte ps) (psDrank ps)
    (psSlept ps) (psPlayed ps) (psGroomed ps) (psEliminated ps) (psConcerns ps)
  where
    (mpid, species, label) = case key of
      KPet pid ->
        let mp = petById roster pid
         in ( Just (petIdText pid)
            , maybe "" (speciesText . petSpecies) mp
            , maybe (petIdText pid) petName mp
            )
      KSpecies sp -> (Nothing, speciesText sp, speciesText sp)
      -- Both the rollup and subjectStatsBetween exclude visitors and persons, so these
      -- arms are unreachable. Label them anyway rather than leave the match partial.
      KVisitor sp -> (Nothing, speciesText sp, speciesText sp)
      KPerson -> (Nothing, "person", "person")

-- | Enrich a set of stored observations by id into 'ObsView's in one batch, so a
-- multi-moment caller like Ask pays one keepsakes scan rather than one per id. Transcripts
-- are left out; the client fetches those on demand.
momentViewsByIds :: App -> UTCTime -> Profile -> [Int64] -> IO [ObsView]
momentViewsByIds app now prof oids = do
  obsById <- Db.getObservationsByIds (appDb app) oids
  ov <- Db.overridesForObsIds (appDb app) oids
  let obss = [o | i <- oids, Just o <- [Map.lookup i obsById]]
  buildObsViews app now prof mempty ov obss

-- | The single-moment read path: enrich one stored observation into an 'ObsView', or
-- 'Nothing' if the id is unknown.
obsViewOf :: App -> UTCTime -> Profile -> Int64 -> IO (Maybe ObsView)
obsViewOf app now prof oid = listToMaybe <$> momentViewsByIds app now prof [oid]

-- | Run a database action reporting success: 200 @{ok:true}@ on 'True', 404 on 'False'.
okOr404 :: Text -> IO Bool -> Handler OkResp
okOr404 msg act = do
  ok <- liftIO act
  if ok then pure (OkResp True) else notFound msg

-- | Run a database read that returns its target if present: 404 with the given message on
-- 'Nothing', otherwise continue with the found value.
foundOr404 :: Text -> IO (Maybe a) -> (a -> Handler b) -> Handler b
foundOr404 msg act k = liftIO act >>= maybe (notFound msg) k

-- | The shape every image and clip endpoint shares: 400 with @badMsg@ if the path is
-- invalid, the looked-up bytes through @project@ if there are any, and 404 with @missMsg@
-- when the source has nothing.
validatedMediaH :: Bool -> Text -> Text -> IO (Maybe a) -> (a -> b) -> Handler b
validatedMediaH valid badMsg missMsg act project
  | valid = foundOr404 missMsg act (pure . project)
  | otherwise = badInput badMsg

-- | Run a model- or service-dependent IO action inside a handler. On failure the
-- action becomes a 503 with @msg@ and its cause traced, rather than warp's bare 500:
-- an @ask@ or @recap@ while the model is offline should read as "unavailable", not
-- "internal error". A domain 'AppError' that surfaced as an exception keeps its own
-- status instead of being flattened to the 503.
orUnavailable :: App -> Text -> IO a -> Handler a
orUnavailable app msg act = do
  outcome <- liftIO (trySync act)
  case outcome of
    Right a -> pure a
    Left e -> case fromException e of
      Just appErr -> throwAppError appErr
      Nothing -> do
        liftIO (traceWith (webTracer (appTracer app)) (UnhandledException (T.pack (show e))))
        throwAppError (Unavailable msg)
