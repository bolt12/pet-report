-- | The @moments@ resource handlers: the faceted browse, the single-moment read, and the
-- per-moment mutations, being review, correct, edit, revert, transcribe, delete and bulk
-- delete.
module PetReport.Web.Moments
  ( momentsH
  , observationH
  , reviewH
  , correctH
  , editH
  , revertH
  , deleteH
  , deleteMomentsH
  , transcribeH
  ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, object, (.=))
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Time.LocalTime      (timeZoneMinutes)
import           Data.Time.Zones          (TZ, timeZoneForUTCTime)
import           Servant                  (Handler)

import           PetReport.App                (App (..))
import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.Profile     (CameraRoom (..), Pet (..),
                                               Profile (..), petById,
                                               resolveCorrection,
                                               uniquePetOfSpecies)
import           PetReport.Domain.Types       (ObsId (..), PetId (..), petIdText,
                                               speciesText)
import           PetReport.Domain.View        (ObsView)
import           PetReport.Domain.Window      (Window (..), parseWindow)
import qualified PetReport.Effect.Db          as Db
import qualified PetReport.Effect.Frigate     as Frigate
import           PetReport.Error              (badInput, notFound)
import           PetReport.Web.Common         (buildObsViews, foundOr404,
                                               nowTzProfile, obsViewOf, okOr404)
import           PetReport.Web.Media          (removeProofFor)
import           PetReport.Web.Types          (CorrectReq, DeleteMomentsReq (..),
                                               EditReq (..), OkResp (..),
                                               ReviewReq (..), correctionTarget)

-- | The faceted, cursor-paged moments browse. Query params resolve server-side against the
-- loaded profile into a 'Db.BrowseQuery': pet to its id plus unique-active-species flag,
-- room to its cameras, time-of-day to a bucket and the zone's current offset. The page's raw
-- moments then enrich exactly as the day view does. Responds @{ items, nextCursor?, total? }@.
momentsH ::
  App ->
  Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text ->
  Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int ->
  Handler Value
momentsH app mfrom mto mpet mact mroom mmedia mtod mreview msearch msort mcursor mlimit = liftIO $ do
  (now, tz, prof) <- nowTzProfile app
  let (bqf, bqt) = case (mfrom, mto) of
        (Nothing, Nothing) -> (Nothing, Nothing)
        _                  -> let w = parseWindow tz now mfrom mto in (Just (winFrom w), Just (winTo w))
      bq =
        Db.emptyBrowseQuery
          { Db.bqFrom = bqf
          , Db.bqTo = bqt
          , Db.bqReview = mreview >>= parseReview
          , Db.bqPet = mpet >>= resolvePet prof
          , Db.bqCameras = fmap (\r -> [camId cr | cr <- cameras prof, room cr == r]) mroom
          , Db.bqActivity = mact
          , Db.bqSearch = msearch
          , Db.bqMedia = mmedia >>= parseMedia
          , Db.bqTimeOfDay = mtod >>= \t -> fmap (\tb -> (tb, tzOffsetSecs tz now)) (timeBucket t)
          , Db.bqSort = if msort == Just "asc" then Db.Asc else Db.Desc
          , Db.bqCursor = mcursor >>= Db.decodeCursor
          , Db.bqLimit = maybe 50 (max 1 . min 200) mlimit
          }
  page <- Db.browseMoments (appDb app) bq
  let obss = Db.bpItems page
  ov <- Db.overridesForObsIds (appDb app) [oid | o <- obss, let ObsId oid = obsId o]
  views <- buildObsViews app now prof mempty ov obss
  pure $
    object $
      ("items" .= views)
        : maybe [] (\c -> ["nextCursor" .= Db.encodeCursor c]) (Db.bpNextCursor page)
          ++ maybe [] (\n -> ["total" .= n]) (Db.bpTotal page)

parseReview :: Text -> Maybe Db.ReviewFilter
parseReview t = case t of
  "reviewed"   -> Just Db.Reviewed
  "unreviewed" -> Just Db.Unreviewed
  "needs-look" -> Just Db.NeedsLook
  _            -> Nothing

parseMedia :: Text -> Maybe Db.MediaKind
parseMedia t = case t of
  "photo" -> Just Db.MediaPhoto
  "clip"  -> Just Db.MediaClip
  "audio" -> Just Db.MediaAudio
  _       -> Nothing

-- | A coarse time-of-day bucket as a half-open local-second range. "night" wraps midnight.
-- The browse arithmetic applies these against the zone offset below.
timeBucket :: Text -> Maybe Db.TimeBucket
timeBucket t = case t of
  "morning"   -> Just (Db.TimeBucket (5 * 3600) (12 * 3600))
  "afternoon" -> Just (Db.TimeBucket (12 * 3600) (17 * 3600))
  "evening"   -> Just (Db.TimeBucket (17 * 3600) (21 * 3600))
  "night"     -> Just (Db.TimeBucket (21 * 3600) (5 * 3600))
  _           -> Nothing

-- | The zone's UTC offset in seconds at a given instant, for the time-of-day facet. A fixed
-- offset, which is the DST approximation 'Db.browseMoments' documents.
tzOffsetSecs :: TZ -> UTCTime -> Int
tzOffsetSecs tz t = timeZoneMinutes (timeZoneForUTCTime tz t) * 60

-- | Resolve a @pet@ facet to a 'Db.PetFilter': the pet's id, plus its species when it is the
-- unique active pet of that species, so unattributed sightings count too. Mirrors
-- 'Db.browseMoments's attribution. An unknown id filters to the explicit overrides alone,
-- which usually means nothing.
resolvePet :: Profile -> Text -> Maybe Db.PetFilter
resolvePet prof name =
  let roster = pets prof
   in case petById roster (PetId name) of
        Nothing -> Just (Db.PetFilter name Nothing)
        Just p ->
          let uniq = case uniquePetOfSpecies roster (petSpecies p) of
                Just up | petId up == petId p -> Just (speciesText (petSpecies p))
                _                             -> Nothing
           in Just (Db.PetFilter (petIdText (petId p)) uniq)

-- | One enriched moment by id, for opening a moment referenced from a keepsake or a pet's
-- last-seen. 404 when the id is unknown.
observationH :: App -> Int64 -> Handler ObsView
observationH app oid =
  foundOr404 "observation not found" load pure
  where
    load = do
      (now, _tz, prof) <- nowTzProfile app
      obsViewOf app now prof oid

reviewH :: App -> ReviewReq -> Handler OkResp
reviewH app (ReviewReq is) =
  liftIO (Db.markReviewed (appDb app) (map fromIntegral is) >> pure (OkResp True))

correctH :: App -> Int64 -> CorrectReq -> Handler OkResp
correctH app oid cr = do
  prof <- liftIO (Db.getProfile (appDb app))
  case resolveCorrection (pets prof) (correctionTarget cr) of
    Nothing -> badInput "unrecognised correction target"
    Just corr -> okOr404 "moment not found" (Db.correctObservation (appDb app) oid corr)

editH :: App -> Int64 -> EditReq -> Handler OkResp
editH app oid (EditReq e) = okOr404 "moment not found" (Db.editObservation (appDb app) oid e)

-- | Undo the owner's review or correction of a moment, reverting to the model's original
-- reading and marking it unreviewed so it can be looked at afresh. 404 for an unknown id.
revertH :: App -> Int64 -> Handler OkResp
revertH app oid = okOr404 "observation not found" (Db.revertObservation (appDb app) oid)

deleteH :: App -> Int64 -> Handler OkResp
deleteH app oid =
  foundOr404 "moment not found" (Db.deleteObservation (appDb app) oid) $ \obs -> do
    liftIO (removeProofFor (appConfig app) obs)
    pure (OkResp True)

-- | Bulk-delete moments, either all of them or those before a date, freeing their proof
-- frames. Returns how many were removed.
deleteMomentsH :: App -> DeleteMomentsReq -> Handler Value
deleteMomentsH app (DeleteMomentsReq mbefore) =
  case traverse parseIso mbefore of
    Left _ -> badInput "invalid date"
    Right cut -> liftIO $ do
      deleted <- Db.deleteObservations (appDb app) cut
      mapM_ (removeProofFor (appConfig app)) deleted
      pure (object ["deleted" .= length deleted])
  where
    parseIso s = maybe (Left ()) Right (iso8601ParseM (T.unpack s) :: Maybe UTCTime)

-- | Transcribe a sound observation's audio via Frigate (Whisper) and cache the
-- text, so a speech moment can show what was said. On demand (Frigate transcribes
-- one at a time); 404 when the observation has no Frigate event, or transcription
-- is disabled / the endpoint is absent.
transcribeH :: App -> Int64 -> Handler Value
transcribeH app oid = do
  -- Frigate gates transcription behind a config flag (and an admin role); when it is off
  -- every call fails, so say so plainly instead of returning an opaque error the owner
  -- reads as a transient glitch.
  enabled <- liftIO (Frigate.transcriptionEnabled (appFrigate app))
  if not enabled
    then badInput "Speech transcription is turned off in Frigate. Enable audio_transcription (and give the app an admin role) to use it."
    else do
      meid <- liftIO (Db.obsEventId (appDb app) oid)
      case meid of
        Nothing -> notFound "This moment has no audio to transcribe."
        Just eid -> do
          r <- liftIO (Frigate.transcribe (appFrigate app) eid)
          case r of
            Right t -> do
              liftIO (Db.setTranscript (appDb app) oid t)
              pure (object ["transcript" .= t])
            Left Frigate.TranscribeRefused -> badInput "Frigate refused the transcription. The app likely needs an admin role in Frigate."
            Left Frigate.NoTranscript -> badInput "No speech could be transcribed from this clip."
