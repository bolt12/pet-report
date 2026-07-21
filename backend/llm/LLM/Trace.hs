-- | Observability seam for the transport: wrap a 'Handle' so every model round-trip emits a
-- typed 'LlmEvent' (request shape, outcome, latency) to an injected sink. The harness
-- defines the event; the app picks the severity and the rendering.
--
-- Failures are traced as well as successes. A call that times out is precisely the one an
-- operator wants the latency and request shape for, and it is invisible from the caller's
-- own error path. The exception is then re-thrown untouched.
module LLM.Trace
  ( LlmEvent (..)
  , ChatOutcome (..)
  , traced
  ) where

import           Control.Exception (SomeException, throwIO)
import           Data.Maybe        (isJust)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Time         (diffUTCTime, getCurrentTime)

import           LLM.Exception     (trySync)
import           LLM.Transport     (AssistantMessage (..), ChatRequest (..),
                                    Handle (..))

-- | What one model round-trip produced. A sum rather than a rendered string, so consumers
-- can count tool-calling turns or filter empty replies without grepping log lines.
data ChatOutcome
  = ToolCalls !Int
  -- ^ The model asked for this many tools before answering.
  | Content
  -- ^ A plain reply with text.
  | NoContent
  -- ^ The model returned neither content nor tool calls.
  | Failed Text
  -- ^ The call did not complete. 'Text' because the transport throws untyped, so the
  -- backend surfaces whatever the HTTP layer raised.
  --
  -- Lazy, unlike the other fields. Rendering an exception is the expensive part, and a
  -- strict field would force it even for an event the log level goes on to discard.
  deriving stock (Eq, Show)

-- | One model round-trip as seen at the transport boundary.
data LlmEvent = LlmChat
  { leStructured :: Bool
  , leTools      :: Int
  , leMaxTokens  :: Int
  , leMillis     :: Int
  , leOutcome    :: ChatOutcome
  }
  deriving stock (Eq, Show)

-- | Wrap a 'Handle' so each 'chat' emits an 'LlmEvent' to @sink@. The timing brackets only
-- the transport round-trip.
--
-- A synchronous failure is traced, then re-thrown unchanged. A cancellation is not a model
-- outcome, so 'trySync' lets it through untraced: a clean shutdown interrupting an
-- in-flight call must not read as the model having failed.
traced :: (LlmEvent -> IO ()) -> Handle -> Handle
traced sink (Handle chat) = Handle $ \req -> do
  t0 <- getCurrentTime
  result <- trySync (chat req)
  t1 <- getCurrentTime
  sink
    LlmChat
      { leStructured = isJust (reqResponseFormat req)
      , leTools = length (reqTools req)
      , leMaxTokens = reqMaxTokens req
      , leMillis = round (diffUTCTime t1 t0 * 1000)
      , leOutcome = either failed answered result
      }
  either throwIO pure result
  where
    answered am = case amToolCalls am of
      tcs@(_ : _) -> ToolCalls (length tcs)
      []
        | isJust (amContent am) -> Content
        | otherwise             -> NoContent
    failed :: SomeException -> ChatOutcome
    failed = Failed . T.pack . show
