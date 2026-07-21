-- | The OpenAI-compatible HTTP backend. It fills an "LLM.Transport" 'Handle' by serialising
-- each request ('requestJson'), POSTing it through an injected 'Poster', and decoding the
-- assistant message out of the @choices[0]@ envelope. A transient socket failure is retried
-- with a capped backoff under the request's retry budget.
module LLM.Backend.OpenAI
  ( Poster
  , openAIHandle
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Exception      (throwIO, try)
import           Data.Aeson             (Value, withObject, (.:))
import           Data.Aeson.Types       (Parser, parseEither)
import           Data.Text              (Text)
import           Data.Time.Clock        (NominalDiffTime)
import           Network.HTTP.Req       (HttpException)

import           LLM.Transport          (AssistantMessage, ChatRequest (..),
                                         Handle (..), requestJson,
                                         retryBackoffMicros)

-- | POST a JSON body to a URL under a per-attempt timeout, returning the decoded JSON
-- response. It must throw 'Network.HTTP.Req.HttpException' on a transient socket-level
-- failure for 'openAIHandle' to retry it; anything else (a config-invalid URL, say)
-- propagates and fails fast. This is the seam that keeps an HTTP client out of the harness.
type Poster = Text -> NominalDiffTime -> Value -> IO Value

-- | Build an OpenAI-compatible 'Handle' from a 'Poster' and an 'STM' source of
-- @(baseUrl, model)@, read fresh per call so a setup change needs no restart. Sends to
-- @baseUrl/v1/chat/completions@ and retries a transient failure under 'reqRetries'. A reply
-- whose envelope does not parse throws.
openAIHandle :: Poster -> STM (Text, Text) -> Handle
openAIHandle poster getUrlModel =
  Handle
    { chat = \r -> do
        (base, model) <- atomically getUrlModel
        let url = base <> "/v1/chat/completions"
            body = requestJson model r
            go n = do
              res <- try (poster url (reqTimeout r) body)
              case res of
                Right v ->
                  either (throwIO . userError . ("invalid chat response: " <>)) pure (parseEither messageFromEnvelope v)
                Left (e :: HttpException)
                  | n > 0 -> threadDelay (retryBackoffMicros (reqRetries r) n) >> go (n - 1)
                  | otherwise -> throwIO e
        go (reqRetries r)
    }

-- | Pull the assistant message out of a chat-completions envelope
-- (@{ "choices": [ { "message": {...} }, ... ] }@). We always request @n=1@, so the first
-- choice is the whole answer.
messageFromEnvelope :: Value -> Parser AssistantMessage
messageFromEnvelope = withObject "chat response" $ \o -> do
  choices <- o .: "choices"
  case choices of
    (c : _) -> withObject "choice" (.: "message") c
    []      -> fail "no choices in response"
