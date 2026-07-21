-- | The app's LLM adapter. The provider-agnostic transport lives in "LLM.Transport"; this
-- module re-exports it and adds the one app-specific piece, 'withHandle', which wires the
-- shared HTTP effect and the configured URL and model newtypes into an OpenAI-compatible
-- 'Handle'.
--
-- The HTTP mechanism stays here rather than in the harness, which lets 'Http.post' stay
-- shared with Frigate, Ntfy and Probe while the harness keeps its dependencies light.
module PetReport.Effect.Llm
  ( module LLM.Transport
  , withHandle
  ) where

import           Control.Concurrent.STM (STM)
import           Data.Aeson             (Value)
import           Data.Bifunctor         (bimap)
import           Data.Text              (Text)
import           Data.Time.Clock        (NominalDiffTime)
import           Network.HTTP.Req       (responseTimeout)

import           LLM.Transport
import qualified LLM.Backend.OpenAI     as OpenAI
import           PetReport.Domain.Types (BaseUrl, ModelName, baseUrlText,
                                         modelNameText)
import qualified PetReport.Effect.Http  as Http
import           PetReport.Util         (microseconds)

-- | Build the app's OpenAI-compatible model 'Handle'. The base URL and model name are read
-- fresh from @getUrlModel@ on every request, so a setup change needs no restart, and the
-- POST goes through the shared 'Http.post'. Both values stay typed until the last moment;
-- the harness sees only their text.
withHandle :: STM (BaseUrl, ModelName) -> (Handle -> IO a) -> IO a
withHandle getUrlModel k =
  k (OpenAI.openAIHandle poster (bimap baseUrlText modelNameText <$> getUrlModel))
  where
    poster :: Text -> NominalDiffTime -> Value -> IO Value
    poster url tmo body = Http.post "model" url [] body (responseTimeout (microseconds tmo))
