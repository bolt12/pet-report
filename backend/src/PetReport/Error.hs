-- | The application's domain error type and its mapping to a structured HTTP response, so a
-- failure returns @{"error": {"code": ..., "message": ...}}@ rather than a bare status code.
-- The codes are the stable contract; the messages are human copy.
module PetReport.Error
  ( AppError (..)
  , errorEnvelope
  , toServerError
  , throwAppError
  , notFound
  , badInput
  , conflict
  ) where

import           Control.Exception    (Exception)
import           Control.Monad.Except (throwError)
import           Data.Aeson           (Value, encode, object, (.=))
import           Data.Text            (Text)
import           Servant              (Handler, ServerError (..), err400, err404,
                                       err409, err503)

-- | The application's domain error, raised either in a handler through 'throwAppError' (the
-- @throwError@ path Servant renders directly) or from effect code as an 'Exception'. The
-- 'Exception' instance is what lets 'PetReport.Web.Common.orUnavailable' recover a typed
-- error that escaped a @liftIO@ with its status intact, instead of flattening it to a 500.
data AppError
  = NotFound Text
  | BadInput Text
  | Conflict Text
  | Unavailable Text
  -- ^ 503: a resource or service the caller asked for is not available right now, e.g.
  -- media that is neither owned nor still at Frigate, or the model being offline.
  deriving stock (Eq, Show)

instance Exception AppError

-- | The nested JSON error body. Both 'toServerError' and the warp-level 500 fallback
-- ('internalErrorResponse') render through here.
errorEnvelope :: Text -> Text -> Value
errorEnvelope code msg = object ["error" .= object ["code" .= code, "message" .= msg]]

-- | Map a domain error to a Servant 'ServerError' carrying the nested JSON body.
toServerError :: AppError -> ServerError
toServerError e =
  base
    { errBody = encode (errorEnvelope code msg)
    , errHeaders = [("Content-Type", "application/json")]
    }
  where
    (base, code, msg) = case e of
      NotFound m    -> (err404, "not_found" :: Text, m)
      BadInput m    -> (err400, "bad_input", m)
      Conflict m    -> (err409, "conflict", m)
      Unavailable m -> (err503, "unavailable", m)

-- | Throw a domain error as its HTTP response from a handler. The per-code throwers below
-- wrap it.
throwAppError :: AppError -> Handler a
throwAppError = throwError . toServerError

-- | Raise the matching domain error from a handler with the given message:
-- 'notFound' a 404, 'badInput' a 400, 'conflict' a 409.
notFound, badInput, conflict :: Text -> Handler a
notFound = throwAppError . NotFound
badInput = throwAppError . BadInput
conflict = throwAppError . Conflict
