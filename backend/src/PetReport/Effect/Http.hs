{-# LANGUAGE RankNTypes #-}

-- | HTTP against a configured base URL. Two things live here: the scheme pick that resolves
-- a base to a concrete http or https request, shared so the config validator cannot accept a
-- URL the runtime then rejects; and the verb combinators the JSON effects build on.
--
-- Every user of the scheme-polymorphic @req@ plumbing comes through here. Ntfy and Probe
-- call 'withResolvedUrl' directly, since their non-JSON bodies and trace-don't-throw
-- handling do not fit the verb combinators.
module PetReport.Effect.Http
  ( withResolvedUrl
  , resolvesToHttp
  , get
  , getBytes
  , put
  , post
  ) where

import           Control.Exception (throwIO)
import           Data.Aeson        (FromJSON, ToJSON)
import           Data.ByteString   (ByteString)
import           Data.Maybe        (isJust)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Network.HTTP.Req  (GET (..), NoReqBody (..), Option, POST (..),
                                    PUT (..), ReqBodyJson (..), Req, Url, bsResponse,
                                    defaultHttpConfig, jsonResponse, req,
                                    responseBody, runReq, useHttpURI, useHttpsURI,
                                    (/:))
import           Text.URI          (URI, mkURI)

-- | Resolve @base@ to http or https and hand the concrete @(Url, Option)@ to a
-- scheme-polymorphic continuation. A URL that does not parse, or parses to neither scheme,
-- runs @onInvalid@ instead; each effect decides whether that throws, traces, or returns a
-- default. The rank-2 continuation keeps the promoted scheme kind implicit at the call
-- sites while letting either branch supply it.
withResolvedUrl ::
  Text ->
  IO a ->
  (forall scheme. Url scheme -> Option scheme -> IO a) ->
  IO a
withResolvedUrl base onInvalid k =
  case mkURI base :: Maybe URI of
    Just uri -> case (useHttpURI uri, useHttpsURI uri) of
      (Just (u, o), _) -> k u o
      (_, Just (u, o)) -> k u o
      _                -> onInvalid
    Nothing -> onInvalid

-- | Whether a URL resolves to http or https, by the SAME predicate the runtime resolver
-- uses, so config validation and 'withResolvedUrl' agree on what counts as valid.
resolvesToHttp :: Text -> Bool
resolvesToHttp u = maybe False ok (mkURI u :: Maybe URI)
  where
    ok uri = isJust (useHttpURI uri) || isJust (useHttpsURI uri)

-- | Resolve @base@, append the path @segs@, and run a scheme-polymorphic request. A base
-- that resolves to neither scheme is a config error rather than a transient fault, so it
-- throws an 'IOException' tagged with @label@. A caller catching
-- 'Network.HTTP.Req.HttpException' still sees it surface instead of degrading silently.
withService ::
  Text ->
  Text ->
  [Text] ->
  (forall scheme. Url scheme -> Option scheme -> Req a) ->
  IO a
withService label base segs act =
  withResolvedUrl base (throwIO (userError ("invalid " <> T.unpack label <> " URL: " <> T.unpack base))) $ \u0 o0 ->
    runReq defaultHttpConfig (act (foldl (/:) u0 segs) o0)

-- | GET @base/segs@ (with the extra options @opt@) and decode the JSON body.
get :: (FromJSON a) => Text -> Text -> [Text] -> (forall scheme. Option scheme) -> IO a
get label base segs opt =
  withService label base segs $ \u o -> responseBody <$> req GET u NoReqBody jsonResponse (o <> opt)

-- | GET @base/segs@ (with the extra options @opt@) and return the raw response bytes.
getBytes :: Text -> Text -> [Text] -> (forall scheme. Option scheme) -> IO ByteString
getBytes label base segs opt =
  withService label base segs $ \u o -> responseBody <$> req GET u NoReqBody bsResponse (o <> opt)

-- | PUT the JSON @body@ to @base/segs@ (with the extra options @opt@) and decode the JSON reply.
put :: (ToJSON b, FromJSON a) => Text -> Text -> [Text] -> b -> (forall scheme. Option scheme) -> IO a
put label base segs body opt =
  withService label base segs $ \u o -> responseBody <$> req PUT u (ReqBodyJson body) jsonResponse (o <> opt)

-- | POST the JSON @body@ to @base/segs@ (with the extra options @opt@) and decode the JSON reply.
post :: (ToJSON b, FromJSON a) => Text -> Text -> [Text] -> b -> (forall scheme. Option scheme) -> IO a
post label base segs body opt =
  withService label base segs $ \u o -> responseBody <$> req POST u (ReqBodyJson body) jsonResponse (o <> opt)
