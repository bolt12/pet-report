-- | The @settings@ write handler: merge an incoming profile while preserving the roster,
-- stamp first-configuration, publish the new Frigate and model URLs to the live handles, and
-- refresh the identification brief.
module PetReport.Web.Settings
  ( settingsPutH
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Servant                (Handler)

import           PetReport.App             (App (..), appJobs, refreshSettings)
import           PetReport.Domain.Profile  (Profile (..))
import qualified PetReport.Effect.Clock    as Clock
import qualified PetReport.Effect.Db       as Db
import           PetReport.Pipeline.Worker (Job (RefreshBrief), submit)

settingsPutH :: App -> Profile -> Handler Profile
settingsPutH app incoming = liftIO $ do
  now <- Clock.now (appClock app)
  -- Preserve the roster. Roster changes go only through the per-pet endpoints, so a
  -- settings save never rewrites the pets. Read and merge inside one transaction, so a
  -- concurrent per-pet edit is not clobbered by writing back a stale roster.
  prof' <- Db.modifyProfile (appDb app) $ \existing ->
    let merged = incoming {pets = pets existing}
     in case configuredAt merged of
          Nothing -> merged {configuredAt = Just now}
          Just _  -> merged
  -- Publish the new Frigate and model URLs to the shared TVar, so the long-lived handles
  -- use them immediately. This has to happen BEFORE submitting RefreshBrief: the worker
  -- regenerates the brief against the model handle, and publishing first stops it
  -- regenerating against the pre-save URL.
  refreshSettings app prof'
  -- Regenerate the cached identification brief in the background when the roster's identity
  -- inputs change; vision, narrative and Ask fall back to petBrief meanwhile. RefreshBrief
  -- re-reads the profile, so this has to come after the modifyProfile above.
  void (submit (appJobs app) RefreshBrief)
  pure prof'
