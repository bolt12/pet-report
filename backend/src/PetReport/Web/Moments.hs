-- | The @moments@ resource handlers: the faceted browse, the single-moment read, and the
-- per-moment mutations, being review, correct, edit, revert, transcribe, delete and bulk
-- delete.
module PetReport.Web.Moments
  ( momentsH
  , observationH
  , reviewH
  , correctH
  , addSightingH
  , removeSightingH
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
import           PetReport.Domain.Profile     (CameraRoom (..), Profile (..),
                                               resolveCorrection)
import           PetReport.Domain.Types       (ObsId (..))
import           PetReport.Domain.View        (ObsView)
import           PetReport.Domain.Window      (Window (..), parseWindow)
import qualified PetReport.Effect.Db          as Db
import qualified PetReport.Effect.Frigate     as Frigate
import           PetReport.Error              (badInput, notFound)
import           PetReport.Web.Common         (buildObsViews, foundOr404,
                                               nowTzProfile, obsViewOf, okOr404)
import           PetReport.Web.Facets         (ActivitySel (..),
                                               BehaviourSel (..), MediaSel (..),
                                               ReviewSel (..), SortSel (..),
                                               SubjectSel, TimeOfDaySel (..),
                                               WellbeingSel (..), resolveSubject)
import           PetReport.Web.Media          (removeProofFor)
import           PetReport.Web.Types          (AddSightingReq, CorrectReq,
                                               DeleteMomentsReq (..),
                                               EditReq (..), OkResp (..),
                                               ReviewReq (..), addedSighting,
                                               correctionTarget)

-- | The faceted, cursor-paged moments browse. Query params resolve server-side against the
-- loaded profile into a 'Db.BrowseQuery': subject to a pet/species/person/visitor filter,
-- room to its cameras, time-of-day to a bucket and the zone's current offset. The page's raw
-- moments then enrich exactly as the day view does. Responds @{ items, nextCursor?, total? }@.
--
-- Each facet arrives already parsed: Servant rejects an unrecognised token with a 400
-- naming the legal set, so no clause is ever silently dropped. The types are all distinct,
-- so two adjacent parameters can no longer be transposed without a compile error.
momentsH ::
  App ->
  Maybe Text -> Maybe Text -> [SubjectSel] -> Maybe ActivitySel ->
  Maybe BehaviourSel -> Maybe WellbeingSel -> Maybe Text -> [Text] ->
  Maybe MediaSel -> Maybe TimeOfDaySel -> Maybe ReviewSel -> Maybe Text ->
  Maybe SortSel -> Maybe Text -> Maybe Int ->
  Handler Value
momentsH app mfrom mto msubjs mact mbeh mwb mroom mcams mmedia mtod mreview msearch msort mcursor mlimit = do
  (now, tz, prof) <- liftIO (nowTzProfile app)
  -- The one facet that needs the roster, and so the one that can still be rejected here
  -- rather than by the parser. Several are AND-ed: "Mochi and a person" returns the frames
  -- holding both, not either.
  subjs <- either badInput pure (traverse (resolveSubject prof) msubjs)
  liftIO $ do
    let (bqf, bqt) = case (mfrom, mto) of
          (Nothing, Nothing) -> (Nothing, Nothing)
          _                  -> let w = parseWindow tz now mfrom mto in (Just (winFrom w), Just (winTo w))
        bq =
          Db.emptyBrowseQuery
            { Db.bqFrom = bqf
            , Db.bqTo = bqt
            , Db.bqReview = (\(ReviewSel r) -> r) <$> mreview
            , Db.bqSubjects = subjs
            -- An explicit camera list wins; otherwise a room resolves to its cameras.
            -- Both end up as the same facet, so a caller never has to know which the
            -- link it followed was built from.
            , Db.bqCameras = case mcams of
                (_ : _) -> Just mcams
                [] -> fmap (\r -> [camId cr | cr <- cameras prof, room cr == r]) mroom
            , Db.bqActivity = (\(ActivitySel a) -> a) <$> mact
            , Db.bqBehaviour = (\(BehaviourSel b) -> b) <$> mbeh
            , Db.bqWellbeing = (\(WellbeingSel w) -> w) <$> mwb
            , Db.bqSearch = msearch
            , Db.bqMedia = (\(MediaSel m) -> m) <$> mmedia
            , Db.bqTimeOfDay = (\(TimeOfDaySel tb) -> (tb, tzOffsetSecs tz now)) <$> mtod
            , Db.bqSort = maybe Db.Desc (\(SortSel s) -> s) msort
            , Db.bqCursor = mcursor >>= Db.decodeCursor
            , Db.bqLimit = maybe 50 (max 1 . min 200) mlimit
            }
    runBrowse app now prof bq

-- | Run an assembled browse and render its page, shared by the handler above.
runBrowse :: App -> UTCTime -> Profile -> Db.BrowseQuery -> IO Value
runBrowse app now prof bq = do
  page <- Db.browseMoments (appDb app) bq
  let obss = Db.bpItems page
  ov <- Db.overridesForObsIds (appDb app) [oid | o <- obss, let ObsId oid = obsId o]
  views <- buildObsViews app now prof mempty ov obss
  pure $
    object $
      ("items" .= views)
        : maybe [] (\c -> ["nextCursor" .= Db.encodeCursor c]) (Db.bpNextCursor page)
          ++ maybe [] (\n -> ["total" .= n]) (Db.bpTotal page)

-- | The zone's UTC offset in seconds at a given instant, for the time-of-day facet. A fixed
-- offset, which is the DST approximation 'Db.browseMoments' documents.
tzOffsetSecs :: TZ -> UTCTime -> Int
tzOffsetSecs tz t = timeZoneMinutes (timeZoneForUTCTime tz t) * 60

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

-- | Retarget one sighting within a moment. The sighting index addresses which subject the
-- owner picked, so a frame holding two cats can have each named separately.
correctH :: App -> Int64 -> Int -> CorrectReq -> Handler OkResp
correctH app oid ix cr = do
  prof <- liftIO (Db.getProfile (appDb app))
  case resolveCorrection (pets prof) (correctionTarget cr) of
    Nothing -> badInput "correction names a pet that is not in the roster"
    Just corr ->
      okOr404
        "moment or sighting not found"
        (Db.correctObservation (appDb app) oid ix corr)

-- | Record a subject the model missed. Appends a sighting, so every existing index and the
-- overrides written against them stay valid.
addSightingH :: App -> Int64 -> AddSightingReq -> Handler OkResp
addSightingH app oid req =
  okOr404
    "moment not found, or it is a sound with no sightings"
    (Db.addObservationSighting (appDb app) oid (addedSighting req))

-- | Drop a subject the model invented. The identity overrides are renumbered with it.
removeSightingH :: App -> Int64 -> Int -> Handler OkResp
removeSightingH app oid ix =
  okOr404 "moment or sighting not found" (Db.removeObservationSighting (appDb app) oid ix)

-- | Edit one sighting's fields. Description and wellbeing are scene-level and apply
-- whichever sighting is addressed; activity, location and behaviour flags land on that
-- sighting alone.
editH :: App -> Int64 -> Int -> EditReq -> Handler OkResp
editH app oid ix (EditReq e) =
  okOr404 "moment or sighting not found" (Db.editObservation (appDb app) oid ix e)

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
