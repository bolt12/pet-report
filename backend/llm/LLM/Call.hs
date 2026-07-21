{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The typed-task layer over "LLM.Transport". A 'Call' bundles what one single-shot model
-- interaction needs: the messages, the sampling knobs, the call budget, and how the reply
-- becomes a typed value. 'run' owns the build/chat/decode plumbing.
--
-- For structured output, 'structured' derives both the @response_format@ schema and the
-- decoder from the result type's 'HasCodec' instance, so naming the type is enough.
-- Free-text tasks use 'prose'. Decoding yields 'Either' a 'DecodeError', which is what
-- 'runRepair' feeds back to the model; 'runMaybe' throws the reason away.
module LLM.Call
  ( Sampling (..)
  , Reply (..)
  , structured
  , prose
  , mapReply
  , refine
  , Call (..)
  , run
  , runMaybe
  , runRepair
  , toRequest
  , system
  , userText
  , userParts
  , extractJson
  , DecodeError (..)
  , note
  ) where

import           Autodocodec        (HasCodec, parseJSONViaCodec)
import           Control.Monad      ((>=>))
import           Data.Aeson         (Value, eitherDecodeStrict)
import           Data.Aeson.Types   (parseEither)
import           Data.Bifunctor     (first)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)

import           LLM.Schema         (codecSchema, jsonSchemaFormat)
import           LLM.Transport      (Sampling (..))
import qualified LLM.Transport      as Llm

-- | Why a model reply could not be turned into the expected value. 'runRepair' appends this
-- text to the conversation and asks again, so word it as something the model can act on.
-- Transport failures do not appear here; they propagate as IO exceptions.
newtype DecodeError = DecodeError
  { decodeErrorText :: Text
  }
  deriving stock (Eq, Show)

-- | Turn a rejected projection into a 'DecodeError'. Lets a post-decode check (blank text,
-- a failed invariant) report through the same channel as a JSON or schema failure.
note :: Text -> Maybe a -> Either DecodeError a
note reason = maybe (Left (DecodeError reason)) Right

-- | How an output type maps to a @response_format@ constraint and a decoder. A record
-- rather than a class, so a task can drop in a bespoke decoder.
data Reply a = Reply
  { replyFormat :: Maybe Value
  -- ^ The @response_format@ value to send, or 'Nothing' for a free-form reply.
  , replyDecode :: Llm.AssistantMessage -> Either DecodeError a
  -- ^ Decode the model's reply into an @a@, or a 'DecodeError' explaining what was wrong.
  }

-- | A structured reply: both the schema and the decoder come from @a@'s 'HasCodec'
-- instance. @name@ labels the schema in the @response_format@ envelope. Decoding tolerates
-- a model that wraps its JSON in prose, and names the stage that failed so 'runRepair' has
-- something to feed back.
structured :: forall a. (HasCodec a) => Text -> Reply a
structured name =
  Reply
    { replyFormat = Just (jsonSchemaFormat name (codecSchema @a))
    , replyDecode = \am -> do
        t <- note "the model returned no content" (Llm.amContent am)
        v <-
          first (\e -> DecodeError ("the reply was not valid JSON: " <> T.pack e))
            (eitherDecodeStrict (encodeUtf8 (extractJson t)) :: Either String Value)
        first (\e -> DecodeError ("the JSON did not match the required schema: " <> T.pack e)) (parseEither parseJSONViaCodec v)
    }

-- | A free-text reply: no @response_format@, the answer is the model's stripped content
-- (empty when the model returned nothing).
prose :: Reply Text
prose = Reply Nothing (fmap T.strip . note "the model returned no content" . Llm.amContent)

-- | Map the decoded value while keeping the wire schema unchanged, e.g. normalise a decoded
-- 'PetReport.Domain.Perception.Scene' or project a one-field wrapper to its 'Text'.
mapReply :: (a -> b) -> Reply a -> Reply b
mapReply f (Reply fmt dec) = Reply fmt (fmap f . dec)

-- | Map with a projection that may reject the value, e.g. a decoded one-field wrapper whose
-- text turns out to be blank. Pair with 'note' to lift a @Maybe@-returning check.
refine :: (a -> Either DecodeError b) -> Reply a -> Reply b
refine f (Reply fmt dec) = Reply fmt (dec >=> f)

-- | A single-shot model call: the messages to send, the sampling knobs, the call budget,
-- and how the reply becomes an @a@.
data Call a = Call
  { callMessages :: [Llm.ChatMessage]
  , callSampling :: Sampling
  , callBudget   :: Llm.CallBudget
  , callReply    :: Reply a
  }

-- | Build the request, send it, decode the reply. A model outage is not caught here; the
-- 'Llm.chat' exception propagates.
run :: Llm.Handle -> Call a -> IO (Either DecodeError a)
run llm c = replyDecode (callReply c) <$> Llm.chat llm (toRequest c)

-- | 'run' with the decode reason discarded, for callers that have their own fallback.
runMaybe :: Llm.Handle -> Call a -> IO (Maybe a)
runMaybe llm c = either (const Nothing) Just <$> run llm c

-- | The wire request a 'Call' sends. Exported so the repair loop can resend it with an
-- amended conversation.
toRequest :: Call a -> Llm.ChatRequest
toRequest c =
  Llm.withBudget (callBudget c) . Llm.withSampling (callSampling c) $
    Llm.defaultRequest
      { Llm.reqMessages       = callMessages c
      , Llm.reqResponseFormat = replyFormat (callReply c)
      }

-- | Run a 'Call'; on a decode failure, append the unusable reply plus a user turn naming
-- what was wrong, then ask again, up to @maxRetries@ extra attempts. This is the
-- instructor/TypeChat repair move: show the model its own error. Schema and sampling stay
-- fixed across attempts. Returns the first value that decodes, or the last 'DecodeError'.
--
-- Not to be confused with the transport retry budget ('LLM.Transport.reqRetries'), which
-- resends the identical request after a socket failure. A transport failure still
-- propagates from here, exactly as in 'run'.
runRepair :: Llm.Handle -> Int -> Call a -> IO (Either DecodeError a)
runRepair llm maxRetries call = go maxRetries (callMessages call)
  where
    go n msgs = do
      am <- Llm.chat llm (toRequest call {callMessages = msgs})
      case replyDecode (callReply call) am of
        Right a -> pure (Right a)
        Left err
          | n <= 0 -> pure (Left err)
          | otherwise -> go (n - 1) (msgs ++ [echo am, correction err])
    echo am =
      Llm.ChatMessage Llm.Assistant (Llm.ContentText (fromMaybe "" (Llm.amContent am))) [] Nothing
    correction err =
      userText
        ( "Your previous reply could not be used: "
            <> decodeErrorText err
            <> ". Please reply again, following the required format exactly."
        )

-- | A system message.
system :: Text -> Llm.ChatMessage
system t = Llm.ChatMessage Llm.System (Llm.ContentText t) [] Nothing

-- | A plain-text user message.
userText :: Text -> Llm.ChatMessage
userText t = Llm.ChatMessage Llm.User (Llm.ContentText t) [] Nothing

-- | A user message of mixed parts (text plus inline images).
userParts :: [Llm.ContentPart] -> Llm.ChatMessage
userParts ps = Llm.ChatMessage Llm.User (Llm.ContentParts ps) [] Nothing

-- | Slice a model reply down to its JSON object (first @{@ to last @}@), tolerating a
-- model that wraps the JSON in prose despite a schema constraint.
extractJson :: Text -> Text
extractJson = T.dropWhileEnd (/= '}') . T.dropWhile (/= '{')
