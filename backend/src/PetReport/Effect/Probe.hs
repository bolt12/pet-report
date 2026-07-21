-- | A reachability probe for the setup "Connect" page: is a base URL answering HTTP with a
-- 2xx? A connection failure, a timeout, and an up-but-broken 4xx or 5xx all count as not
-- reachable.
module PetReport.Effect.Probe
  ( reachable
  ) where

import           Control.Exception     (SomeException, try)
import           Data.Either           (fromRight)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Network.HTTP.Req      (GET (..), HttpConfig (..), NoReqBody (..),
                                        defaultHttpConfig, ignoreResponse, req,
                                        responseStatusCode, responseTimeout, runReq,
                                        (/:))

import           PetReport.Effect.Http (withResolvedUrl)

-- | @reachable base segs@ is 'True' when a GET to @base@ (with path @segs@)
-- returns a 2xx response within a short timeout.
reachable :: Text -> [Text] -> IO Bool
reachable base segs = fromRight False <$> tryProbe
  where
    tryProbe = try go :: IO (Either SomeException Bool)
    -- Disable req's non-2xx-throws so every HTTP status returns normally, then judge
    -- reachability from the status code here.
    cfg = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
    go =
      withResolvedUrl base (ioError (userError ("invalid URL: " <> T.unpack base))) $ \u o ->
        runReq cfg $ do
          r <- req GET (foldl (/:) u segs) NoReqBody ignoreResponse (o <> responseTimeout (5 * 1000000))
          let c = responseStatusCode r
          pure (c >= 200 && c < 300)
