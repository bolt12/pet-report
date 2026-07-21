-- | The camera-scoped read endpoints: the configured-camera list for setup auto-discovery,
-- the live Frigate and model reachability status, a camera's latest live frame, and the
-- content-addressed proof frames.
module PetReport.Web.Cameras
  ( camerasH
  , statusH
  , frameH
  , proofH
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM   (readTVarIO)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, object, (.=))
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Servant                  (Handler)

import           PetReport.App            (App (..), EffSettings (..), appSettings)
import           PetReport.Domain.Profile (CameraRoom (..), Profile (..))
import           PetReport.Domain.Types   (baseUrlText)
import qualified PetReport.Effect.Db      as Db
import qualified PetReport.Effect.Frigate as Frigate
import qualified PetReport.Effect.Probe   as Probe
import           PetReport.Web.Common     (validatedMediaH)
import           PetReport.Web.Media      (readProof, validCam, validProofFile)
import           PetReport.Web.Types      (CameraInfo (..), Cached, cached)

-- | Cameras Frigate has configured, each with its live online status. Backs the setup
-- wizard's auto-discovery.
camerasH :: App -> Handler [CameraInfo]
camerasH app = liftIO $ do
  names <- Frigate.listCameras (appFrigate app)
  online' <- Frigate.onlineCameras (appFrigate app) names
  pure [CameraInfo n (n `elem` online') | n <- names]

-- | Live reachability of the configured Frigate and vision model, plus per-camera online
-- status, for the setup "Connect" page. Reads the current profile URLs, so the page reflects
-- freshly-saved edits.
statusH :: App -> Handler Value
statusH app = liftIO $ do
  prof <- Db.getProfile (appDb app)
  -- Read the effective URLs the handles actually use, published to the TVar on profile
  -- save, rather than re-deriving the profile-over-env overlay here. The Connect page can
  -- then never show a URL the app is not using.
  urls <- readTVarIO (appSettings app)
  let frigUrl = baseUrlText (efFrigate urls)
      modUrl = baseUrlText (efLlm urls)
  -- Two independent network probes, run together so the Connect page's status does not
  -- wait for one and then the other.
  (fReach, mReach) <-
    Async.concurrently
      (Probe.reachable frigUrl ["api", "version"])
      (Probe.reachable modUrl ["v1", "models"])
  online <- Frigate.onlineCameras (appFrigate app) (map camId (cameras prof))
  let camObj c =
        object
          [ "name" .= camId c
          , "room" .= room c
          , "enabled" .= enabled c
          , "online" .= (camId c `elem` online)
          ]
  pure $
    object
      [ "frigate" .= object ["url" .= frigUrl, "reachable" .= fReach]
      , "model" .= object ["url" .= modUrl, "reachable" .= mReach]
      , "cameras" .= map camObj (cameras prof)
      ]

frameH :: App -> Text -> Handler ByteString
frameH app cam =
  validatedMediaH (validCam cam) "invalid camera name" "no live frame for this camera"
    (Frigate.latestFrame (appFrigate app) cam) id

proofH :: App -> Text -> Text -> Handler (Cached ByteString)
proofH app cam file =
  validatedMediaH (validCam cam && validProofFile file) "invalid proof path"
    "proof frame not found" (readProof (appConfig app) cam file) cached
