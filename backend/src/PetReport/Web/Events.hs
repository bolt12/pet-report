-- | The @events@ media endpoints: an event's snapshot and clip, served owned-first. Our
-- stored copy of a kept moment's media comes first, with a live proxy to Frigate behind it,
-- so a kept moment outlives Frigate's retention.
module PetReport.Web.Events
  ( eventSnapH
  , eventClipH
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Servant         (Handler)

import           PetReport.App            (App (..))
import qualified PetReport.Effect.Frigate as Frigate
import           PetReport.Web.Common     (validatedMediaH)
import           PetReport.Web.Media      (eventMediaBytes, validEventId)
import           PetReport.Web.Types      (Cached, cached)

-- | Serve an event snapshot or clip owned-first: our stored copy, written when the moment
-- was kept, otherwise a live proxy to Frigate. One URL covers both, so a kept moment
-- outlives Frigate's retention while an un-kept one still streams live, from the same origin
-- as the API under Vite in dev and nginx in prod. 404 when the id is malformed or neither
-- source has the media.
eventSnapH :: App -> Text -> Handler (Cached ByteString)
eventSnapH app eid =
  validatedMediaH (validEventId eid) "invalid event id" "snapshot unavailable"
    (eventMediaBytes (appConfig app) eid ".jpg" (Frigate.eventSnapshot (appFrigate app))) cached

eventClipH :: App -> Text -> Handler (Cached ByteString)
eventClipH app eid =
  validatedMediaH (validEventId eid) "invalid event id" "clip unavailable"
    (eventMediaBytes (appConfig app) eid ".mp4" (Frigate.eventClip (appFrigate app))) cached
