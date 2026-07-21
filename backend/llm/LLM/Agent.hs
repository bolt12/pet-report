-- | A bounded tool-calling loop: the harness half of an "agent". Each turn sends the
-- conversation so far. A reply with no tool calls is the answer; a reply asking for tools
-- runs them, appends the results, and goes round again. An iteration budget and a
-- wall-clock deadline both stop it, so a slow model gives up its best answer so far
-- instead of pinning the caller's thread.
--
-- Nothing here is app-specific. A 'Tool' carries its own handler, and whatever side
-- output the tools produce (observation ids, cited sources) accumulates in a monoid @w@.
module LLM.Agent
  ( Tool (..)
  , toolSpec
  , RunBudget (..)
  , AgentTurn (..)
  , runAgent
  , renderResult
  ) where

import           Data.Aeson         (Value, decodeStrict, object, (.=))
import           Data.Aeson.Text    (encodeToLazyText)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy     as TL
import           Data.Time          (UTCTime)

import           LLM.Exception      (trySync)
import qualified LLM.Transport      as Llm

-- | One tool the agent may call: its wire name and description, the JSON schema its
-- arguments must satisfy (derive it with 'LLM.Schema.codecSchema'), and the handler. The
-- handler's 'Value' result is what the model reads back; @w@ collects side output, and is
-- @()@ for a tool that has none.
data Tool w = Tool
  { toolName   :: Text
  , toolDesc   :: Text
  , toolSchema :: Value
  , toolRun    :: Value -> IO (Value, w)
  }

-- | The wire spec the model sees for a tool.
toolSpec :: Tool w -> Llm.ToolSpec
toolSpec t = Llm.ToolSpec (toolName t) (toolDesc t) (toolSchema t)

-- | What stops a run. The first two are checked before every turn.
data RunBudget = RunBudget
  { rbMaxIters           :: Int
  -- ^ Maximum tool-calling iterations before the loop stops with the model's last reply.
  , rbDeadline           :: UTCTime
  -- ^ Wall-clock instant after which the loop returns the best answer it has.
  , rbMaxToolResultChars :: Int
  -- ^ Backstop cap on one tool result's serialised size. An oversize result is refused
  -- with a "narrow your query" signal ('renderResult'), never truncated.
  }

-- | Request knobs stamped on every model turn. Constant across a run; the loop threads
-- the conversation and the tool list itself.
data AgentTurn = AgentTurn
  { atSampling :: Llm.Sampling
  , atBudget   :: Llm.CallBudget
  }

-- | Run the loop over an initial conversation, returning the answer plus the accumulated
-- side output. @clock@ is checked against 'rbDeadline' before each turn; @fallback@ stands
-- in when the model produces no answer at all.
--
-- The two failure modes differ on purpose. A model outage propagates as an exception,
-- while a tool that throws becomes an error result the model can read and react to.
runAgent
  :: (Monoid w)
  => Llm.Handle
  -> IO UTCTime
  -> RunBudget
  -> AgentTurn
  -> Text
  -> [Tool w]
  -> [Llm.ChatMessage]
  -> IO (Text, w)
runAgent llm clock budget turn fallback tools msgs0 =
  go msgs0 mempty (rbMaxIters budget)
  where
    specs        = map toolSpec tools
    dispatch     = Map.fromList [(toolName t, toolRun t) | t <- tools]
    request msgs =
      Llm.withBudget (atBudget turn) . Llm.withSampling (atSampling turn) $
        Llm.defaultRequest
          { Llm.reqMessages = msgs
          , Llm.reqTools    = specs
          }
    go msgs surfaced n = do
      now <- clock
      if now >= rbDeadline budget
        then pure (lastAnswer fallback msgs, surfaced)
        else do
          am <- Llm.chat llm (request msgs)
          case Llm.amToolCalls am of
            [] -> pure (finalText fallback am, surfaced)
            tcs
              | n <= 0 -> pure (finalText fallback am, surfaced)
              | otherwise -> do
                  let assistant =
                        Llm.ChatMessage Llm.Assistant
                          (Llm.ContentText (fromMaybe "" (Llm.amContent am)))
                          tcs
                          Nothing
                  outcomes <- mapM (runToolCall (rbMaxToolResultChars budget) dispatch) tcs
                  let results = map fst outcomes
                      surfaced' = surfaced <> foldMap snd outcomes
                  go (msgs ++ [assistant] ++ results) surfaced' (n - 1)

-- | Run one tool call: decode its arguments, dispatch to the handler, wrap the result as a
-- tool-role message. Failures are caught here, so one bad call degrades to an error the
-- model can read rather than aborting the answer. A failed or unknown tool has no side
-- output.
runToolCall :: (Monoid w) => Int -> Map Text (Value -> IO (Value, w)) -> Llm.ToolCall -> IO (Llm.ChatMessage, w)
runToolCall maxChars dispatch tc = do
  let argsVal = fromMaybe (object [])
                          (decodeStrict (TE.encodeUtf8 (Llm.tcArgs tc)))
  (result, extra) <-
    case Map.lookup (Llm.tcName tc) dispatch of
      Just h -> do
        r <- trySync (h argsVal)
        pure $ case r of
          Right vi -> vi
          Left e   -> (object ["error" .= show e], mempty)
      Nothing -> pure (object ["error" .= ("unknown tool: " <> Llm.tcName tc)], mempty)
  pure (Llm.ChatMessage Llm.ToolRole (Llm.ContentText (renderResult maxChars result)) [] (Just (Llm.tcId tc)), extra)

-- | Serialise a tool result for the model, refusing rather than slicing when it exceeds
-- @maxChars@. Truncating serialised JSON mid-string would hand the model a malformed blob
-- that still looks structured, so an over-cap result becomes a valid-JSON sentinel naming
-- the limit and telling the model how to narrow its query.
--
-- @maxChars@ counts characters, a cheap stand-in for tokens. It is a backstop; tools are
-- expected to bound their own output well below it.
renderResult :: Int -> Value -> Text
renderResult maxChars result
  | T.length rendered <= maxChars = rendered
  | otherwise =
      TL.toStrict . encodeToLazyText $
        object
          [ "error" .= ("tool_result_too_large" :: Text)
          , "chars" .= T.length rendered
          , "limit" .= maxChars
          , "hint" .= ("The result was too large to return. Narrow it with a smaller day or time range, a keyword, or a count query, then call the tool again." :: Text)
          ]
  where
    rendered = TL.toStrict (encodeToLazyText result)

-- | The best answer when the deadline stops the loop mid-flight: the most recent non-blank
-- assistant text, or @fallback@ if there isn't one yet.
lastAnswer :: Text -> [Llm.ChatMessage] -> Text
lastAnswer fallback msgs =
  case [ t
       | Llm.ChatMessage Llm.Assistant (Llm.ContentText t) _ _ <- msgs
       , not (T.null (T.strip t))
       ] of
    [] -> fallback
    ts -> T.strip (last ts)

-- | The answer from a final (no-tool-call) reply, or @fallback@ if the model produced
-- nothing usable.
finalText :: Text -> Llm.AssistantMessage -> Text
finalText fallback am = case Llm.amContent am of
  Just t | not (T.null (T.strip t)) -> T.strip t
  _                                 -> fallback
