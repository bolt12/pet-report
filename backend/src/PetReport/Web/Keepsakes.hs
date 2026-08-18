-- | The @keepsakes@ resource: the gallery read, keeping and un-keeping a moment, and the
-- owned-media lifecycle that lets a kept moment outlive Frigate's retention. Keeping copies
-- the still and clip; un-keeping drops the copy.
module PetReport.Web.Keepsakes
  ( keepsakesH
  , keepsakeAddH
  , keepsakeDelH
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value, object, (.=))
import           Data.Int               (Int64)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import           Servant                (Handler)
import           System.Directory       (createDirectoryIfMissing)

import           PetReport.App                (App (..))
import           PetReport.Config             (Config (..))
import           PetReport.Domain.Observation (FrigateMeta (..), origin,
                                               originMeta)
import           PetReport.Domain.Profile     (Profile (..))
import           PetReport.Domain.Types       (EventId (..))
import           PetReport.Domain.View        (proofRetainDays)
import qualified PetReport.Effect.Clock       as Clock
import qualified PetReport.Effect.Db          as Db
import qualified PetReport.Effect.Frigate     as Frigate
import           PetReport.View.Enrich        (mkKeepsake)
import           PetReport.Web.Media          (removeOwnedMedia, saveOwnedMedia)
import           PetReport.Web.Types          (KeepsakeReq (..), OkResp (..))

-- | The keepsakes gallery, optionally scoped to one pet, each enriched to a thumbnail from
-- its own moment. The @{ items }@ shape can carry paging, but returns no cursor while the
-- set stays small.
keepsakesH :: App -> Maybe Text -> Handler Value
keepsakesH app mpet = liftIO $ do
  now <- Clock.now (appClock app)
  prof <- Db.getProfile (appDb app)
  ks <- Db.listKeepsakes (appDb app) mpet
  -- One batched read for every keepsake's moment, rather than a pooled single-row read
  -- per keepsake. The pool has only a few connections, so N keepsakes would serialize.
  obsById <- Db.getObservationsByIds (appDb app) (map Db.kObsId ks)
  let crs = cameras prof
      items =
        [ mkKeepsake proofRetainDays crs now k obs
        | k <- ks
        , Just obs <- [Map.lookup (Db.kObsId k) obsById]
        ]
  pure (object ["items" .= items])

-- | Keep a moment: insert the 'Db.Keepsake', then take ownership of its media by copying
-- the still and clip out of Frigate while they still exist, so the keepsake outlives
-- Frigate's retention.
keepsakeAddH :: App -> Int64 -> KeepsakeReq -> Handler Db.Keepsake
keepsakeAddH app oid kr = liftIO $ do
  now <- Clock.now (appClock app)
  k <- Db.insertKeepsake (appDb app) oid (krPetId kr) (krCaption kr) now
  ownKeptMedia app oid
  pure k

-- | Un-keep a moment: delete the keepsake and drop the owned copy of its media, reverting
-- the moment to borrowed, retention-bound Frigate media.
keepsakeDelH :: App -> Int64 -> Handler OkResp
keepsakeDelH app kid = liftIO $ do
  -- deleteKeepsake returns the moment id it referenced, which saves a second lookup.
  moid <- Db.deleteKeepsake (appDb app) kid
  mapM_ (freeOwnedMedia app) moid
  pure (OkResp True)

-- | Copy a kept event moment's still and clip from Frigate into our owned media store, keyed
-- by event id, so the keepsake survives Frigate pruning. Best-effort: it stores whatever
-- Frigate still has, and only for the media the event actually carries. The countdown is
-- what urges keeping in time. A periodic sample has no event media, and its proof frame is
-- protected from pruning instead (see 'Pipeline.pruneProof').
ownKeptMedia :: App -> Int64 -> IO ()
ownKeptMedia app oid = do
  mobs <- Db.getObservation (appDb app) oid
  case originMeta . origin =<< mobs of
    Nothing -> pure ()
    Just m -> do
      let EventId eid = eventId m
          cfg = appConfig app
      createDirectoryIfMissing True (cfgMediaDir cfg)
      when (detectorHasSnapshot m) $
        saveOwnedMedia cfg eid ".jpg" =<< Frigate.eventSnapshot (appFrigate app) eid
      when (detectorHasClip m) $
        saveOwnedMedia cfg eid ".mp4" =<< Frigate.eventClip (appFrigate app) eid

-- | Remove a moment's owned media copies, by its observation id. Called on un-keep;
-- moment deletion frees them through 'removeProofFor'.
freeOwnedMedia :: App -> Int64 -> IO ()
freeOwnedMedia app oid = do
  mobs <- Db.getObservation (appDb app) oid
  mapM_ (removeOwnedMedia (appConfig app)) (originMeta . origin =<< mobs)
