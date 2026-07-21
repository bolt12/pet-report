-- | A small, typed, provider-agnostic model transport. It holds the wire types for an
-- OpenAI-compatible chat API (roles, messages, tools, @response_format@, the tool-call round
-- trip, and the llama.cpp @chat_template_kwargs@ and @reasoning_content@ extensions the
-- generic libraries don't model), the pure request serialisation ('requestJson'), the call
-- budget, and the 'Handle' a caller runs against.
--
-- "LLM.Backend.OpenAI" fills a 'Handle' against a real endpoint. A test fills one with a
-- stub.
module LLM.Transport
  ( Handle (..)
  , Role (..)
  , ContentPart (..)
  , Content (..)
  , ChatMessage (..)
  , ToolSpec (..)
  , ToolCall (..)
  , AssistantMessage (..)
  , ChatRequest (..)
  , defaultRequest
  , requestJson
  , retryBackoffMicros
  , CallBudget (..)
  , interactiveBudget
  , backgroundBudget
  , briefBudget
  , withBudget
  , Sampling (..)
  , withSampling
  ) where

import           Data.Aeson      (FromJSON (..), ToJSON (..), Value (..), object,
                                  withObject, (.!=), (.:), (.:?), (.=))
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)

-- --------------------------------------------------------------------------- --
-- Wire types
-- --------------------------------------------------------------------------- --

-- | Who authored a chat turn (mapped to the OpenAI role strings by 'roleText').
data Role = System | User | Assistant | ToolRole

roleText :: Role -> Text
roleText r = case r of
  System    -> "system"
  User      -> "user"
  Assistant -> "assistant"
  ToolRole  -> "tool"

-- | One part of a multi-part message body: a text span or an inline image.
data ContentPart
  = TextPart Text -- ^ a plain text span
  | ImagePart Text -- ^ a @data:image/jpeg;base64,...@ URL

instance ToJSON ContentPart where
  toJSON (TextPart t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ImagePart u) =
    object
      [ "type" .= ("image_url" :: Text)
      , "image_url" .= object ["url" .= u]
      ]

-- | A message body: plain text, or a list of parts (text plus inline images).
data Content
  = ContentText Text
  | ContentParts [ContentPart]

instance ToJSON Content where
  toJSON (ContentText t)   = String t
  toJSON (ContentParts ps) = toJSON ps

-- | A tool call as emitted by the model and echoed back on the next turn. The
-- @arguments@ are a raw JSON string, decoded lazily by the caller.
data ToolCall = ToolCall
  { tcId   :: Text
  -- ^ The call id, echoed back on the matching tool-result turn.
  , tcName :: Text
  -- ^ The tool the model wants to run.
  , tcArgs :: Text
  -- ^ Its arguments as a raw JSON string, decoded by the caller.
  }

instance ToJSON ToolCall where
  toJSON tc =
    object
      [ "id" .= tcId tc
      , "type" .= ("function" :: Text)
      , "function" .= object ["name" .= tcName tc, "arguments" .= tcArgs tc]
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o -> do
    i <- o .:? "id" .!= ""
    fn <- o .: "function"
    n <- fn .: "name"
    a <- fn .:? "arguments" .!= "{}"
    pure (ToolCall i n a)

-- | One turn on the wire: who spoke, what they said, plus the tool-call bookkeeping
-- that threads a tool result back to the call that requested it.
data ChatMessage = ChatMessage
  { msgRole       :: Role
  -- ^ Who is speaking (system, user, assistant, or tool).
  , msgContent    :: Content
  -- ^ The turn's content (text, or text-plus-image parts).
  , msgToolCalls  :: [ToolCall]
  -- ^ On an assistant turn, the tool calls it made; dropped from the wire when empty.
  , msgToolCallId :: Maybe Text
  -- ^ On a tool-result turn, the id of the call this answers.
  }

instance ToJSON ChatMessage where
  toJSON m =
    object $
      [ "role" .= roleText (msgRole m)
      , "content" .= msgContent m
      ]
        ++ ["tool_calls" .= msgToolCalls m | not (null (msgToolCalls m))]
        ++ maybe [] (\i -> ["tool_call_id" .= i]) (msgToolCallId m)

-- | A tool offered to the model: a name, a description it reads to decide when to
-- call, and a JSON schema for the arguments.
data ToolSpec = ToolSpec
  { toolName        :: Text
  -- ^ The tool's name, matched against 'tcName' on a call.
  , toolDescription :: Text
  -- ^ What the tool does, so the model can decide when to use it.
  , toolParameters  :: Value
  -- ^ JSON schema for the arguments.
  }

instance ToJSON ToolSpec where
  toJSON t =
    object
      [ "type" .= ("function" :: Text)
      , "function"
          .= object
            [ "name" .= toolName t
            , "description" .= toolDescription t
            , "parameters" .= toolParameters t
            ]
      ]

-- | The model's reply: its text answer, its reasoning trace (when thinking was on),
-- and any tool calls it wants run. Any of the three may be absent on a given turn.
data AssistantMessage = AssistantMessage
  { amContent   :: Maybe Text
  -- ^ The answer text, if the model produced one this turn.
  , amReasoning :: Maybe Text
  -- ^ The @reasoning_content@ trace, present only when thinking was enabled.
  , amToolCalls :: [ToolCall]
  -- ^ Tools the model asked to run before answering; empty on a plain reply.
  }

instance FromJSON AssistantMessage where
  parseJSON = withObject "AssistantMessage" $ \o ->
    AssistantMessage
      <$> o .:? "content"
      <*> o .:? "reasoning_content"
      <*> (fromMaybe [] <$> o .:? "tool_calls")

-- --------------------------------------------------------------------------- --
-- Request
-- --------------------------------------------------------------------------- --

-- | A model chat request: the conversation plus generation limits and the llama.cpp
-- extensions (tools, @response_format@, thinking). Build from 'defaultRequest' and
-- adjust the fields you need; 'withBudget' stamps the timeout and retry count.
data ChatRequest = ChatRequest
  { reqMessages       :: [ChatMessage]
  -- ^ The conversation so far, oldest first (system prompt, then user/tool turns).
  , reqMaxTokens      :: Int
  -- ^ Upper bound on generated tokens; with thinking on it must also cover the
  -- reasoning trace, not only the answer.
  , reqTemperature    :: Double
  -- ^ Sampling temperature: low for extraction/JSON, higher for prose.
  , reqTools          :: [ToolSpec]
  -- ^ Tools the model may call; when empty, no @tools@ field is sent.
  , reqResponseFormat :: Maybe Value
  -- ^ A JSON schema to constrain the reply (llama.cpp @response_format@); 'Nothing'
  -- leaves the output free-form.
  , reqEnableThinking :: Bool
  -- ^ Enable the model's reasoning pass (llama.cpp @chat_template_kwargs@); the
  -- trace returns in @reasoning_content@ and counts against 'reqMaxTokens'.
  , reqTimeout        :: NominalDiffTime
  -- ^ Per-attempt HTTP timeout.
  , reqRetries        :: Int
  -- ^ Extra attempts after the first on a transient (socket-level) failure.
  }

-- | A request with sensible defaults; set 'reqMessages' and adjust as needed.
defaultRequest :: ChatRequest
defaultRequest =
  ChatRequest
    { reqMessages       = []
    , reqMaxTokens      = 500 -- a floor; every real caller overrides it
    , reqTemperature    = 0.3
    , reqTools          = []
    , reqResponseFormat = Nothing
    , reqEnableThinking = False
    , reqTimeout        = 150 -- seconds; enough for a llama-swap model (re)load
    , reqRetries        = 0
    }

-- | The wire payload for a request. This is where the llama.cpp-specific shape lives:
-- @chat_template_kwargs@ always, @tools@ and @tool_choice@ only when tools are set,
-- @response_format@ only when a schema is. Pure, so a test can pin the exact JSON the server
-- sees without an HTTP call.
requestJson :: Text -> ChatRequest -> Value
requestJson model r =
  object $
    [ "model" .= model
    , "messages" .= reqMessages r
    , "max_tokens" .= reqMaxTokens r
    , "temperature" .= reqTemperature r
    , "chat_template_kwargs" .= object ["enable_thinking" .= reqEnableThinking r]
    ]
      ++ ( if null (reqTools r)
             then []
             else ["tools" .= reqTools r, "tool_choice" .= ("auto" :: Text)]
         )
      ++ maybe [] (\rf -> ["response_format" .= rf]) (reqResponseFormat r)

-- --------------------------------------------------------------------------- --
-- Retry backoff
-- --------------------------------------------------------------------------- --

-- | Capped exponential backoff between retries: 2s, then 4s, capped at 8s. The retry stays
-- in-thread and the serve process runs one serial worker, so a long pause stalls every
-- queued job behind it.
--
-- A llama-swap model reload needs no pause at all: llama-swap holds the request open while
-- it swaps, which 'reqTimeout' already covers. This backoff only has to ride out a
-- socket-level failure.
retryBackoffMicros :: Int -> Int -> Int
retryBackoffMicros total n =
  let used = max 0 (total - n)
   in min 8 (2 ^ (used + 1)) * 1000000

-- --------------------------------------------------------------------------- --
-- Call budgets
-- --------------------------------------------------------------------------- --

-- | A per-call timeout and retry count, picked by whether an owner is waiting. Each budget
-- nests under the deadline enclosing it: the client timeout for interactive calls, the batch
-- wall-clock for background ones.
data CallBudget = CallBudget
  { cbTimeout :: NominalDiffTime
  -- ^ Per-attempt HTTP timeout.
  , cbRetries :: Int
  -- ^ Extra attempts after the first on a transient (socket-level) failure.
  }

-- | An owner is waiting on a warp thread: one attempt under a ~100s cap, inside the ~120s
-- client timeout, then an honest error rather than a hung thread.
interactiveBudget :: CallBudget
interactiveBudget = CallBudget 100 0

-- | The serial worker, nobody waiting: a longer cap plus one retry to ride out a slow model
-- or a blip, still under the batch's wall-clock deadline.
backgroundBudget :: CallBudget
backgroundBudget = CallBudget 180 1

-- | A background call that reasons over the whole roster, so more generous than
-- 'backgroundBudget'. Still under the batch deadline.
briefBudget :: CallBudget
briefBudget = CallBudget 600 1

-- | Stamp a request with a budget's timeout and retry count. Nothing else changes.
withBudget :: CallBudget -> ChatRequest -> ChatRequest
withBudget b r =
  r
    { reqTimeout = cbTimeout b
    , reqRetries = cbRetries b
    }

-- --------------------------------------------------------------------------- --
-- Sampling
-- --------------------------------------------------------------------------- --

-- | The generation knobs of a single model turn: how random, how long, and whether the
-- reasoning pass is on. Stamped onto a request by 'withSampling', the companion to
-- 'withBudget'.
data Sampling = Sampling
  { samplingTemperature :: Double
  -- ^ Sampling temperature: low for extraction/JSON, higher for prose.
  , samplingMaxTokens   :: Int
  -- ^ Upper bound on generated tokens (must also cover a thinking trace when enabled).
  , samplingThinking    :: Bool
  -- ^ Whether to enable the model's reasoning pass.
  }

-- | Stamp a request with a sampling temperature, token cap, and thinking flag. Nothing else
-- changes.
withSampling :: Sampling -> ChatRequest -> ChatRequest
withSampling s r =
  r
    { reqTemperature = samplingTemperature s
    , reqMaxTokens = samplingMaxTokens s
    , reqEnableThinking = samplingThinking s
    }

-- --------------------------------------------------------------------------- --
-- Handle
-- --------------------------------------------------------------------------- --

-- | The model client. 'chat' sends a request and returns the assistant's message, throwing
-- on an HTTP failure that outlasts the retries. "LLM.Backend.OpenAI" fills it against a real
-- endpoint; a test fills it with a stub.
newtype Handle = Handle
  { chat :: ChatRequest -> IO AssistantMessage
  }
