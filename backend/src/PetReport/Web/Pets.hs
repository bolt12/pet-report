-- | The @pets@ resource handlers: per-pet insights (as of a chosen day) and the
-- roster CRUD (describe, add, edit, photo, archive/unarchive, delete).
module PetReport.Web.Pets
  ( insightsH
  , petDescribeH
  , petAddH
  , petEditH
  , petPhotoH
  , petArchiveH
  , petUnarchiveH
  , petDeleteH
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value, object, (.=))
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Text              (Text)
import           Data.Time              (UTCTime, addDays, addUTCTime)
import           Servant                (Handler)

import qualified PetReport.Analysis.Vision    as Vision
import           PetReport.App                (App (..), appJobs)
import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.PetReport   (BalanceV (..), PetInsights (..),
                                               Spot (..), WellbeingV (..),
                                               anomalyChip, insightsForAll,
                                               monthStatFor, monthStatPairs,
                                               monthTrends)
import           PetReport.Domain.Profile     (CameraRoom (..), Pet (..),
                                               Profile (..), activePets, petById)
import           PetReport.Domain.Stats       (psSightings)
import           PetReport.Domain.Trends      (DayTrend)
import           PetReport.Domain.Types       (PetId (..), Species (..),
                                               petIdText)
import           PetReport.Domain.Window      (Window (..), localDayOf,
                                               parseWindow, recognizedArg,
                                               startOfLocalDay)
import qualified PetReport.Effect.Clock       as Clock
import qualified PetReport.Effect.Db          as Db
import           PetReport.Error              (AppError (..), badInput, conflict,
                                               notFound, throwAppError)
import qualified PetReport.Pipeline           as Pipeline
import           PetReport.Pipeline.Worker    (Job (RefreshBrief), submit)
import           PetReport.View.Enrich        (mkKeepsake, mkRecap, wellbeingLine)
import           PetReport.Web.Common         (foundOr404, nowTzProfile,
                                               orUnavailable)
import           PetReport.Web.Media          (decodePhoto, petPhotoBytes,
                                               removePetPhoto, removeProofFor,
                                               resolvePetPhoto)
import           PetReport.Web.Types          (AddPetReq (..), Cached,
                                               DescribeReq (..), DescribeResp (..),
                                               EditPetReq (..), OkResp (..),
                                               applyPetEdit, cached)

-- | Per-pet insights as of a chosen local day, wrapped as the contract's
-- @{ asOf, pets }@. @asOf@ absent means today.
insightsH :: App -> Maybe Text -> Handler Value
insightsH app masOf = do
  let d = fromMaybe "today" masOf
  ins <- petsAtH app d
  pure (object ["asOf" .= d, "pets" .= ins])

-- | Per-pet insights as of a chosen local day, usually today. The insights anchor at a time
-- INSIDE that day, so the glance tiles and the trailing week and 30-day windows are computed
-- as of the selected day rather than the calendar present. That is what lets the Pets page
-- page back through history.
petsAtH :: App -> Text -> Handler [PetInsights]
petsAtH app d
  | not (recognizedArg d) = badInput "invalid day"
  | otherwise = liftIO $ do
      (now, tz, prof) <- nowTzProfile app
      let w = parseWindow tz now (Just d) (Just d)
          selDay = localDayOf tz (winFrom w)
          -- winTo is the next local midnight, whose localDay is the following day, so step
          -- back a second to land the anchor on the selected day.
          anchor = if localDayOf tz now == selDay then now else addUTCTime (-1) (winTo w)
          roster = pets prof
          crs = cameras prof
          weekStart = startOfLocalDay tz (addDays (-6) selDay)
          monthStart = addUTCTime (negate (30 * 86400)) anchor
      -- Load overrides and observations over the whole month ONCE. The month is a superset
      -- of the week, so the week is a pure in-memory filter with no second round trip that
      -- re-fetches and re-decodes every week row. Reusing one override map for both windows
      -- also keeps the 30-day tiles on the same attribution as the week, so
      -- day <= week <= month holds by construction.
      ov <- Db.overridesBetween (appDb app) monthStart anchor
      monthObs <- Db.observationsBetween (appDb app) monthStart anchor
      let weekObs = filter ((>= weekStart) . at) monthObs
          -- Run the two per-day trends passes ONCE for the whole roster rather than once
          -- per pet. 'insightsForAll' shares the week pass and 'monthTrends' the month
          -- pass, and each pet reads its own KPet bucket out of them.
          tiles = activePets prof
          insAll = insightsForAll tz ov crs roster anchor weekObs tiles
          monthTrs = monthTrends tz ov roster anchor monthObs
      -- One tile per active pet. The full roster is still threaded in for identity, so an
      -- archived pet's old corrections resolve to its name.
      mapM (enrichInsights app crs anchor monthTrs) insAll

-- | Fill one pet's precomputed deterministic insights with the database-sourced parts,
-- summary and keepsake, plus the month totals. 'petsAtH' has already computed the week and
-- month trends ONCE for the whole roster and threaded them in, so this runs no per-pet trend
-- pass: only the pet's own reads and its slice of the shared month trends.
enrichInsights ::
  App ->
  [CameraRoom] ->
  UTCTime ->
  [DayTrend] ->
  (Pet, PetInsights) ->
  IO PetInsights
enrichInsights app crs now monthTrs (pet, ins) = do
  msum <- Db.getPetSummary (appDb app) (piId ins)
  ks <- Db.listKeepsakes (appDb app) (Just (piId ins))
  -- Enrich the shown keepsake from its own moment, fetched by id so it still works for a
  -- keepsake older than the loaded week, which gets the card a real thumbnail.
  keepsake <- case listToMaybe ks of
    Nothing -> pure Nothing
    Just k -> do
      mobs <- Db.getObservation (appDb app) (Db.kObsId k)
      pure (mkKeepsake Pipeline.proofRetainDays crs now k <$> mobs)
  let monthStat = monthStatFor monthTrs pet
      weekSeen = sum (piSpark ins)
      daysSeen = length (filter (> 0) (piSpark ins))
      topSpot = case piSpots ins of (s : _) -> Just (spRoom s); _ -> Nothing
      wbLine = wellbeingLine (piName ins) (wbKind (piWellbeing ins)) weekSeen (blRestPct (piBalance ins)) topSpot
  pure
    ins
      { piWellbeing = (piWellbeing ins) {wbText = wbLine}
      , piRecap = msum >>= mkRecap weekSeen daysSeen
      , piKeepsake = keepsake
      , piMonthSeen = psSightings monthStat
      , piMonthStats = monthStatPairs monthStat
      , piAnomaly = anomalyChip (piSeen ins) monthStat
      }


-- | Draft a physical description from a single photo, with no persistence and no pet id, so
-- the setup wizard can use it before the pet is saved. 400 on an unreadable image, 503 when
-- the model returns nothing usable.
petDescribeH :: App -> DescribeReq -> Handler DescribeResp
petDescribeH app (DescribeReq photo sp mExisting) =
  case decodePhoto photo of
    Nothing -> badInput "the photo is missing, unreadable, or too large"
    Just jpg -> do
      mDesc <-
        orUnavailable app "the model is unavailable right now; please try again" $
          Vision.describePet (appLlm app) (Species sp) mExisting jpg
      maybe
        (throwAppError (Unavailable "the model could not describe this photo; please try again"))
        (pure . DescribeResp)
        mDesc

-- | Add a pet to the roster, 409 if the id is taken, and regenerate the identification brief
-- since the roster's identity inputs changed.
petAddH :: App -> AddPetReq -> Handler Pet
petAddH app (AddPetReq newPet mPhoto) = do
  -- The uniqueness test runs inside the write transaction, so two concurrent adds of the
  -- same id cannot both pass and both append. A plain read-then-write would race.
  res <- liftIO $ Db.modifyProfileE (appDb app) $ \p ->
    if any ((== petId newPet) . petId) (pets p)
      then Left ()
      else Right p {pets = pets p ++ [newPet]}
  case res of
    Left () -> conflict "a pet with that id already exists"
    Right _ -> liftIO $ do
      -- Store the photo only once the id is claimed, so a duplicate add can never clobber
      -- an existing pet's avatar file. The token is recorded in a second write, and a
      -- failed image write leaves the pet photoless.
      tok <- resolvePetPhoto (appConfig app) (petIdText (petId newPet)) False mPhoto Nothing
      mapM_ (Db.modifyProfile (appDb app) . stampPhoto (petId newPet)) tok
      void (submit (appJobs app) RefreshBrief)
      pure newPet {petPhoto = tok}

-- | Edit a pet's fields in place, 404 if absent, then refresh the brief.
petEditH :: App -> Text -> EditPetReq -> Handler Pet
petEditH app pid req = do
  prof <- liftIO (Db.getProfile (appDb app))
  case petById (pets prof) (PetId pid) of
    Nothing -> notFound "pet not found"
    Just existing -> liftIO $ do
      -- Resolve the avatar, storing, replacing or clearing it, and fold the new token into
      -- the same write as the field edits.
      tok <- resolvePetPhoto (appConfig app) pid (epPhotoRemove req) (epPhoto req) (petPhoto existing)
      let updated = (applyPetEdit req existing) {petPhoto = tok}
      _ <- Db.modifyProfile (appDb app) (\p -> p {pets = map (replacePet updated) (pets p)})
      void (submit (appJobs app) RefreshBrief)
      pure updated
  where
    replacePet up x = if petId x == PetId pid then up else x

-- | Record a pet's freshly-stored avatar token in the profile, matched by id. The add path
-- needs a separate write, because the roster append that claims the id runs before the photo
-- is stored. The edit path folds it into its own write instead.
stampPhoto :: PetId -> Text -> Profile -> Profile
stampPhoto pid t p = p {pets = map set (pets p)}
  where
    set x = if petId x == pid then x {petPhoto = Just t} else x

-- | Serve a pet's avatar photo, 404 if the pet has none. The client appends a @?v=<token>@
-- that changes with the image, which is what makes the immutable cache header safe.
petPhotoH :: App -> Text -> Handler (Cached ByteString)
petPhotoH app pid =
  foundOr404 "no photo for this pet" (petPhotoBytes (appConfig app) pid) (pure . cached)

-- | Archive a pet, stamped now, so it drops out of auto-identification while its history is
-- kept. 404 if the id is unknown.
petArchiveH :: App -> Text -> Handler OkResp
petArchiveH app pid = do
  now <- liftIO (Clock.now (appClock app))
  setArchived app pid (Just now)

-- | Un-archive a pet, restoring it to the active roster. 404 if the id is unknown.
petUnarchiveH :: App -> Text -> Handler OkResp
petUnarchiveH app pid = setArchived app pid Nothing

-- | Set or clear a pet's archived stamp, 404 if absent, then refresh the brief since an
-- archived pet no longer auto-identifies.
setArchived :: App -> Text -> Maybe UTCTime -> Handler OkResp
setArchived app pid mAt = do
  prof <- liftIO (Db.getProfile (appDb app))
  case petById (pets prof) (PetId pid) of
    Nothing -> notFound "pet not found"
    Just _ -> liftIO $ do
      _ <- Db.modifyProfile (appDb app) (\p -> p {pets = map stamp (pets p)})
      void (submit (appJobs app) RefreshBrief)
      pure (OkResp True)
  where
    stamp x = if petId x == PetId pid then x {petArchivedAt = mAt} else x

-- | Permanently erase a pet: the moments tied to it, its keepsakes, summary, and
-- drop it from the profile (freeing the moments' proof frames on disk). The only
-- path that deletes a pet's history.
petDeleteH :: App -> Text -> Handler OkResp
petDeleteH app pid = liftIO $ do
  -- Erase the pet's data and drop it from the roster in one transaction, so a
  -- crash cannot leave the data gone yet the pet still listed (or the reverse).
  deleted <- Db.purgePetAndProfile (appDb app) pid $ \p ->
    p {pets = filter ((/= PetId pid) . petId) (pets p)}
  -- Proof frames and the avatar on disk are not transactional; free them after the
  -- commit.
  mapM_ (removeProofFor (appConfig app)) deleted
  removePetPhoto (appConfig app) pid
  pure (OkResp True)

