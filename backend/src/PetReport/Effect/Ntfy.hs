-- | The ntfy push-notification effect, fire-and-forget. A push failure is logged, never
-- thrown, since a missed notification must not abort a batch run.
module PetReport.Effect.Ntfy
  ( Handle (..)
  , Notification (..)
  , withHandle
  ) where

import           Control.Exception  (SomeException, try)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Req      (POST (..), ReqBodyBs (..),
                                        defaultHttpConfig, header, ignoreResponse,
                                        req, runReq)
import           PetReport.Effect.Http (withResolvedUrl)
import           PetReport.Trace       (NtfyEvent (..), Tracer, traceWith)

-- | One push message, mapped onto ntfy's publish headers. 'notifPriority' is ntfy's 1-5
-- scale; 'notifTags' is a comma-separated list ntfy renders as emoji, e.g.
-- @paw_prints,warning@.
data Notification = Notification
  { notifTitle    :: Text
  , notifBody     :: Text
  , notifPriority :: Int
  , notifTags     :: Text
  }

newtype Handle = Handle {push :: Notification -> IO ()}

-- | @withHandle tracer url mClick@ where @mClick@ is an optional tap-through URL.
withHandle :: Tracer IO NtfyEvent -> Text -> Maybe Text -> (Handle -> IO a) -> IO a
withHandle tracer url mClick k = k Handle {push = pushImpl tracer url mClick}

pushImpl :: Tracer IO NtfyEvent -> Text -> Maybe Text -> Notification -> IO ()
pushImpl tracer url mClick n = do
  r <- try (send tracer url mClick n) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left e   -> traceWith tracer (PushFailed (T.pack (show e)))

-- | Resolve the topic URL as http or https, both of which config validation accepts. An
-- invalid URL is traced, never thrown; push stays fire-and-forget.
send :: Tracer IO NtfyEvent -> Text -> Maybe Text -> Notification -> IO ()
send tracer url mClick n =
  withResolvedUrl url (traceWith tracer (InvalidNtfyUrl url)) $ \u o ->
    runReq defaultHttpConfig $ do
      _ <-
        req
          POST
          u
          (ReqBodyBs (TE.encodeUtf8 (T.take 3500 (notifBody n))))
          ignoreResponse
          ( o
              <> header "Title" (TE.encodeUtf8 (headerText (notifTitle n)))
              <> header "Priority" (TE.encodeUtf8 (T.pack (show (notifPriority n))))
              <> header "Tags" (TE.encodeUtf8 (notifTags n))
              <> maybe mempty (header "Click" . TE.encodeUtf8) mClick
          )
      pure ()

-- | The Title header carries owner-controlled pet names, so strip control characters (a
-- newline would split the HTTP request) and cap the length.
headerText :: Text -> Text
headerText = T.take 200 . T.filter (\ch -> ch >= ' ' && ch /= '\DEL')
