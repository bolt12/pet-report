-- | A stored observation. 'Origin' carries the Frigate metadata only for events,
-- so "a periodic sample has an event id" is unrepresentable. The derived
-- presentation view (identity, chips, media URLs) lives in 'PetReport.Domain.View'.
module PetReport.Domain.Observation
  ( FrigateMeta (..)
  , Origin (..)
  , originMeta
  , Observation (..)
  , NewObservation (..)
  ) where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime)
import           GHC.Generics                (Generic)
import           PetReport.Domain.Perception (Perception)
import           PetReport.Domain.Types      (Camera, EventId, ObsId)

data FrigateMeta = FrigateMeta
  { eventId         :: EventId
  , detectorLabel   :: Text
  , detectorScore   :: Double
  , detectorHasClip :: Bool
  -- ^ Whether Frigate saved a video clip for this event. A snapshot-only event
  -- has none, so the media view must offer a photo, not a clip URL that 404s.
  , detectorHasSnapshot :: Bool
  -- ^ Whether Frigate saved a still for this event. Clips and snapshots sit on
  -- independent switches, so an event can genuinely have one without the other, and the
  -- media view must not advertise a snapshot URL that would 404.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Origin
  = PeriodicSample
  | FromEvent FrigateMeta
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The Frigate metadata an origin carries: 'Just' for an event, 'Nothing' for a periodic
-- sample. The column projections all read from this one @Maybe@, so "a sample has no meta"
-- is stated once.
originMeta :: Origin -> Maybe FrigateMeta
originMeta PeriodicSample = Nothing
originMeta (FromEvent m)  = Just m

-- Every field is banged. Observations are decoded a day or a month at a time, so a lazy
-- field would keep one thunk per row alive for as long as the list is. Maybe/Origin fields
-- force to the constructor, not its contents, so absence stays absence.
data Observation = Observation
  { obsId      :: !ObsId
  , at         :: !UTCTime
  , camera     :: !Camera
  , origin     :: !Origin
  , perception :: !Perception
  , reviewed   :: !Bool
  -- ^ Whether the owner has confirmed this moment, directly or as a side effect of a
  -- correction or edit. An uncertain moment stays in the needs-look backlog until
  -- reviewed.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | An observation on the insert path, before storage. SQLite assigns 'obsId', and a
-- freshly-ingested observation is never pre-reviewed, so neither field exists yet. Keeping
-- this separate from 'Observation' means no in-memory value ever carries a placeholder id.
data NewObservation = NewObservation
  { noAt         :: !UTCTime
  , noCamera     :: !Camera
  , noOrigin     :: !Origin
  , noPerception :: !Perception
  }
  deriving stock (Eq, Show, Generic)
