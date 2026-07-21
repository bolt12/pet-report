-- | A stored daily/period narrative summary.
module PetReport.Domain.Report
  ( Period (..)
  , periodText
  , Report (..)
  ) where

import           Data.Aeson             (FromJSON (..), ToJSON (..),
                                         genericParseJSON, genericToJSON)
import           Data.Text              (Text)
import           Data.Time              (Day, UTCTime)
import           GHC.Generics           (Generic)
import           PetReport.Domain.Types (enumOptions)

data Period
  = Morning
  | Evening
  deriving stock (Eq, Show, Enum, Bounded, Generic)

instance ToJSON Period where
  toJSON = genericToJSON enumOptions

instance FromJSON Period where
  parseJSON = genericParseJSON enumOptions

-- | A period's wire text, matching its JSON encoding. Adding a constructor makes this an
-- incomplete match, so the build catches a missing case.
periodText :: Period -> Text
periodText Morning = "morning"
periodText Evening = "evening"

data Report = Report
  { reportDay :: Day
  , period    :: Period
  , narrative :: Text
  , reportAt  :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
