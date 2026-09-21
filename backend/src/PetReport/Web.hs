{-# LANGUAGE DataKinds #-}

-- | The Servant HTTP API and warp runner behind the @serve@ subcommand. nginx serves the
-- SPA, so this backend is a pure JSON API plus the guarded @/proof@ and @/api/frame@ image
-- endpoints. Handlers close over 'App' and run in Servant's 'Handler'.
--
-- Observations come back as enriched 'ObsView's, with identity, chips, room and media all
-- derived here, so the client never re-implements domain logic.
module PetReport.Web
  ( runServer
  , resolveRefresh
  , dayH
  , observationH
  , transcribeH
  , overviewH
  , statusH
  , batchH
  , keepsakeAddH
  , keepsakeDelH
  , eventSnapH
  , eventClipH
  , DayResponse (..)
  , PresenceV (..)
  , CameraInfo (..)
  , AskResp (..)
  , KeepsakeReq (..)
  , momentsH
  , petAddH
  , petEditH
  , petPhotoH
  , petArchiveH
  , AddPetReq (..)
  , EditPetReq (..)
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=))
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.String              (fromString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Network.Wai.Handler.Warp (defaultSettings,
                                           defaultShouldDisplayException,
                                           runSettings, setHost, setOnException,
                                           setOnExceptionResponse, setPort)
import           Servant

import           Network.HTTP.Types.Status     (status500, statusCode)
import           Network.Wai                   (Middleware, Response,
                                                rawPathInfo, requestMethod,
                                                responseLBS, responseStatus)
import           PetReport.App                 (App (..), RepairScope (..),
                                                appJobs, repairStoredPetNames)
import           PetReport.Config              (Config (..), parseListen,
                                                portNumber)
import           PetReport.Domain.Profile      (Pet, Profile (..),
                                                enabledCameras, roomOf)
import           PetReport.Domain.Types        (Camera (..))
import           PetReport.Domain.View         (ObsView)
import           PetReport.Domain.Window       (localDayOf)
import qualified PetReport.Effect.Clock        as Clock
import qualified PetReport.Effect.Db           as Db
import qualified PetReport.Effect.Frigate      as Frigate
import           PetReport.Error               (envelopeFormatters, errorEnvelope)
import qualified PetReport.Analysis.IdentGuide as IdentGuide
import qualified PetReport.Pipeline            as Pipeline
import           PetReport.Pipeline.Retention  (runRetentionPoller)
import           PetReport.Pipeline.Scheduler  (runBatchScheduler,
                                                runCaptureScheduler)
import           PetReport.Pipeline.Worker     (Job (..), runJobs)
import           PetReport.Trace               (Tracer, WebEvent (..),
                                                pipelineTracer, traceWith,
                                                webTracer)
import           PetReport.Web.Cameras
import           PetReport.Web.Cleanup
import           PetReport.Web.Days
import           PetReport.Web.Events
import           PetReport.Web.Facets
import           PetReport.Web.Keepsakes
import           PetReport.Web.Moments
import           PetReport.Web.Ops
import           PetReport.Web.Pets
import           PetReport.Web.Settings
import           PetReport.Web.Types

-- --------------------------------------------------------------------------- --
-- API
-- --------------------------------------------------------------------------- --

-- Each resource's endpoints are grouped into a named sub-API, and 'type API' composes them.
-- Within a group, static segments precede their sibling captures, so @pets/describe@ is not
-- swallowed by @pets/:id@. The groups have distinct path prefixes, so their relative order
-- does not affect routing.

type RootAPI =
       "api" :> "health"   :> Get '[PlainText] Text
  :<|> "api" :> "overview" :> Get '[JSON] Value

type DaysAPI =
  "api" :> "days" :> Capture "date" Text :> Get '[JSON] DayResponse

type MomentsAPI =
       -- Every facet has its own type, so an unrecognised token is a 400 naming the legal
       -- set rather than a silently dropped clause, and two adjacent params can no longer
       -- be transposed without a compile error.
       "api" :> "moments"
         :> QueryParam "from" Text           :> QueryParam "to" Text
         :> QueryParams "subject" SubjectSel :> QueryParam "activity" ActivitySel
         :> QueryParam "behaviour" BehaviourSel :> QueryParam "wellbeing" WellbeingSel
         -- room is a saved label from the profile; camera is the raw id, which is what a
         -- favourite-spot link carries because its label may be a display-only fallback.
         :> QueryParam "room" Text           :> QueryParams "camera" Text
         :> QueryParam "media" MediaSel
         :> QueryParam "timeOfDay" TimeOfDaySel :> QueryParam "review" ReviewSel
         :> QueryParam "search" Text         :> QueryParam "sort" SortSel
         :> QueryParam "cursor" Text         :> QueryParam "limit" Int
         :> Get '[JSON] Value
  :<|> "api" :> "moments" :> "review"                            :> ReqBody '[JSON] ReviewReq :> Post '[JSON] OkResp
  :<|> "api" :> "moments" :> "delete"                            :> ReqBody '[JSON] DeleteMomentsReq :> Post '[JSON] Value
  -- Both address ONE sighting: a moment holding two cats and a sitter is three sightings,
  -- and each is named and edited on its own. The index is in the path rather than the body
  -- so it cannot be defaulted away.
  -- Add a subject the model missed, or drop one it invented. Together with the
  -- per-sighting correction below, this is what lets a frame hold a pet AND a person
  -- rather than one or the other.
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "sightings" :> ReqBody '[JSON] AddSightingReq :> Post '[JSON] OkResp
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "sightings" :> Capture "ix" Int :> Delete '[JSON] OkResp
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "sightings" :> Capture "ix" Int :> "correction" :> ReqBody '[JSON] CorrectReq :> Post '[JSON] OkResp
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "sightings" :> Capture "ix" Int :> "edit"       :> ReqBody '[JSON] EditReq :> Post '[JSON] OkResp
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "revert"      :> Post '[JSON] OkResp
  -- Take back the verdict without throwing the owner's corrections away. The narrower
  -- half of revert, and the one an accidental "that's right" wants.
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "unreview"    :> Post '[JSON] OkResp
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "keepsake"    :> ReqBody '[JSON] KeepsakeReq :> Post '[JSON] Db.Keepsake
  :<|> "api" :> "moments" :> Capture "id" Int64 :> "transcript"  :> Post '[JSON] Value
  :<|> "api" :> "moments" :> Capture "id" Int64                  :> Get '[JSON] ObsView
  :<|> "api" :> "moments" :> Capture "id" Int64                  :> Delete '[JSON] OkResp

type KeepsakesAPI =
       "api" :> "keepsakes" :> QueryParam "pet" Text :> Get '[JSON] Value
  :<|> "api" :> "keepsakes" :> Capture "id" Int64    :> Delete '[JSON] OkResp

type PetsAPI =
       "api" :> "pets" :> "insights" :> QueryParam "asOf" Text :> Get '[JSON] Value
  :<|> "api" :> "pets" :> "describe"                           :> ReqBody '[JSON] DescribeReq :> Post '[JSON] DescribeResp
  :<|> "api" :> "pets"                                         :> ReqBody '[JSON] AddPetReq :> Post '[JSON] Pet
  :<|> "api" :> "pets" :> Capture "id" Text                    :> ReqBody '[JSON] EditPetReq :> Patch '[JSON] Pet
  :<|> "api" :> "pets" :> Capture "id" Text :> "photo"         :> Get '[JPEG] (Cached ByteString)
  :<|> "api" :> "pets" :> Capture "id" Text :> "archive"       :> Post '[JSON] OkResp
  :<|> "api" :> "pets" :> Capture "id" Text :> "unarchive"     :> Post '[JSON] OkResp
  :<|> "api" :> "pets" :> Capture "id" Text                    :> Delete '[JSON] OkResp

type CamerasAPI =
       "api" :> "cameras"                                      :> Get '[JSON] [CameraInfo]
  :<|> "api" :> "cameras" :> "status"                          :> Get '[JSON] Value
  :<|> "api" :> "cameras" :> Capture "cam" Text :> "frame"     :> Get '[JPEG] ByteString

type SettingsAPI =
       "api" :> "settings" :> Get '[JSON] Profile
  :<|> "api" :> "settings" :> ReqBody '[JSON] Profile :> Put '[JSON] Profile

type CleanupAPI =
       "api" :> "cleanup"                                          :> Get '[JSON] Value
  :<|> "api" :> "cleanup" :> "preview" :> QueryParam "days" Int    :> Get '[JSON] Value
  :<|> "api" :> "cleanup"                                          :> Post '[JSON] Value

type OpsAPI =
       "api" :> "model-stats"                                      :> Get '[JSON] Value
  :<|> "api" :> "refresh" :> "status" :> QueryParam "day" Text     :> Get '[JSON] Value
  :<|> "api" :> "ask"                                              :> ReqBody '[JSON] AskReq :> Post '[JSON] AskResp
  :<|> "api" :> "recap"                                            :> ReqBody '[JSON] RecapReq :> Post '[JSON] RecapResp
  :<|> "api" :> "refresh"                                          :> ReqBody '[JSON] RefreshReq :> Post '[JSON] RefreshResp

type EventsAPI =
       "api" :> "events" :> Capture "eid" Text :> "snapshot.jpg" :> Get '[JPEG] (Cached ByteString)
  :<|> "api" :> "events" :> Capture "eid" Text :> "clip.mp4"     :> Get '[MP4] (Cached ByteString)

type ProofAPI =
  "proof" :> Capture "cam" Text :> Capture "file" Text :> Get '[JPEG] (Cached ByteString)

type API =
       RootAPI
  :<|> DaysAPI
  :<|> MomentsAPI
  :<|> KeepsakesAPI
  :<|> PetsAPI
  :<|> CamerasAPI
  :<|> SettingsAPI
  :<|> CleanupAPI
  :<|> OpsAPI
  :<|> EventsAPI
  :<|> ProofAPI


-- --------------------------------------------------------------------------- --
-- Server
-- --------------------------------------------------------------------------- --

server :: App -> Server API
server app =
       -- RootAPI
       (pure "ok" :<|> overviewH app)
       -- DaysAPI
  :<|> dayH app
       -- MomentsAPI
  :<|> ( momentsH app
    :<|> reviewH app
    :<|> deleteMomentsH app
    :<|> addSightingH app
    :<|> removeSightingH app
    :<|> correctH app
    :<|> editH app
    :<|> revertH app
    :<|> unreviewH app
    :<|> keepsakeAddH app
    :<|> transcribeH app
    :<|> observationH app
    :<|> deleteH app
       )
       -- KeepsakesAPI
  :<|> (keepsakesH app :<|> keepsakeDelH app)
       -- PetsAPI
  :<|> ( insightsH app
    :<|> petDescribeH app
    :<|> petAddH app
    :<|> petEditH app
    :<|> petPhotoH app
    :<|> petArchiveH app
    :<|> petUnarchiveH app
    :<|> petDeleteH app
       )
       -- CamerasAPI
  :<|> (camerasH app :<|> statusH app :<|> frameH app)
       -- SettingsAPI
  :<|> (liftIO (Db.getProfile (appDb app)) :<|> settingsPutH app)
       -- CleanupAPI
  :<|> (cleanupInfoH app :<|> gcPreviewH app :<|> cleanupNowH app)
       -- OpsAPI
  :<|> (modelStatsH app :<|> batchH app :<|> askH app :<|> recapH app :<|> refreshH app)
       -- EventsAPI
  :<|> (eventSnapH app :<|> eventClipH app)
       -- ProofAPI
  :<|> proofH app


-- | The overview: enabled cameras with live online status and the earliest logged day.
-- Reads the profile fresh rather than the startup-baked config,
-- so cameras added in setup appear immediately in the long-lived @serve@ process.
overviewH :: App -> Handler Value
overviewH app = liftIO $ do
  prof <- Db.getProfile (appDb app)
  tz <- Clock.timeZone (appClock app)
  let cams = enabledCameras prof
  online <- Frigate.onlineCameras (appFrigate app) cams
  -- The earliest logged local day, so the client stops the day navigator at the first day
  -- with data instead of a fixed cap. Omitted when there is none.
  mRange <- Db.tsRange (appDb app)
  let earliest = case mRange of
        Just (lo, _) -> ["earliestDay" .= T.pack (show (localDayOf tz lo))]
        Nothing      -> []
      one c =
        object
          [ "camera" .= c
          , "room" .= roomOf (cameras prof) (Camera c)
          , "online" .= (c `elem` online)
          ]
  pure (object (("cameras" .= map one cams) : earliest))

-- --------------------------------------------------------------------------- --
-- Helpers + runner
-- --------------------------------------------------------------------------- --

runServer :: App -> IO ()
runServer app = do
  let listen = cfgListen (appConfig app)
      tr = webTracer (appTracer app)
      -- Unreachable in practice, since loadConfig rejects an address parseListen cannot
      -- read. Kept total so runServer never partial-matches.
      (host, port) = case parseListen listen of
        Just (h, p) -> (T.unpack h, portNumber p)
        Nothing     -> ("127.0.0.1", 8116)
  -- Before anything is served, so no reader sees a species that was really a pet's name.
  repairStoredPetNames WhenStale app
  traceWith tr (Listening listen)
  -- Every piece of pet-report's automation is a background loop of THIS process, each under
  -- 'withAsync' so shutdown tears it down and 'link' surfaces a loop bug: the job worker for
  -- on-demand refreshes and rebuilds, the retention poller keeping the countdown in step
  -- with Frigate, and the capture and batch schedulers driving the periodic frame capture
  -- and the twice-daily analysis, report and cleanup.
  withBackgroundLoops
    [ runJobs (appJobs app) (pipelineTracer (appTracer app)) (performJob app)
    , runRetentionPoller app
    , runCaptureScheduler app
    , runBatchScheduler app
    ]
    $ runSettings
      ( setHost (fromString host)
          . setPort port
          -- Turn an uncaught exception on a handler path, the model going down mid-ask
          -- say, into our JSON error envelope and log the real cause, rather than warp's
          -- bare 500 with the reason lost.
          . setOnException (\_ e -> when (defaultShouldDisplayException e) (traceWith tr (UnhandledException (T.pack (show e)))))
          . setOnExceptionResponse (const internalErrorResponse)
          $ defaultSettings
      )
      -- serveWithContext, only for the error formatters: Servant's own rejections (a
      -- malformed body, an unparseable query) then answer in the same JSON envelope the
      -- handlers use, instead of raw parser prose the SPA cannot read.
      ( traceRequests
          tr
          ( serveWithContext
              (Proxy :: Proxy API)
              (envelopeFormatters :. EmptyContext)
              (server app)
          )
      )

-- | The response for an uncaught exception on a handler path: our JSON error envelope, so
-- the SPA renders it like any other error rather than warp's bare 500. 'setOnException' logs
-- the real cause; the client never sees it.
internalErrorResponse :: Response
internalErrorResponse =
  responseLBS
    status500
    [("Content-Type", "application/json")]
    (encode (errorEnvelope "internal" "something went wrong"))

-- | Start each background loop under 'withAsync' and 'link', so a loop bug propagates rather
-- than dying silently, then run @act@ with them all alive. They are cancelled when @act@
-- returns or throws.
withBackgroundLoops :: [IO ()] -> IO a -> IO a
withBackgroundLoops [] act = act
withBackgroundLoops (loop : rest) act =
  Async.withAsync loop $ \a -> Async.link a >> withBackgroundLoops rest act

-- | Run a queued background job. Total, so 'runJobs' can trace failures around it.
-- 'RefreshBrief' re-reads the latest saved profile, so it always regenerates against the
-- current roster.
performJob :: App -> Job -> IO ()
performJob app job = case job of
  RunBatch     -> Pipeline.batch app
  BuildDay d   -> Pipeline.buildDay app d
  RefreshBrief -> do
    prof <- Db.getProfile (appDb app)
    IdentGuide.refreshIfStale (appLlm app) (appDb app) prof

-- | Trace every request: method, path, status. The event renders at 'Debug' for a 2xx or
-- 3xx and 'Warning' for a 4xx or 5xx, so the flood from live-frame and batch polling stays
-- hidden unless @PET_REPORT_LOG_LEVEL=debug@, while failures show at the default level.
traceRequests :: Tracer IO WebEvent -> Middleware
traceRequests tr next rq sendRes = next rq $ \res -> do
  traceWith
    tr
    (RequestServed (decodeUtf8 (requestMethod rq)) (decodeUtf8 (rawPathInfo rq)) (statusCode (responseStatus res)))
  sendRes res

