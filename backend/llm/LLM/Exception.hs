-- | Exception discipline for the harness: the line between a synchronous failure it may
-- observe (a tool handler that threw, a transport that gave up) and an asynchronous one it
-- must never absorb (a shutdown cancellation, a thread kill). Both functions rest on one
-- fact: an asynchronous exception is wrapped in 'SomeAsyncException' by
-- @asyncExceptionToException@, so a single check covers the whole class, @async@'s
-- @AsyncCancelled@ included.
--
-- "PetReport.Util" has its own copy. The harness takes no dependency on the app, so both
-- sides of that seam keep their own predicate.
module LLM.Exception
  ( isAsync
  , trySync
  ) where

import           Control.Exception (SomeAsyncException, SomeException,
                                    fromException, throwIO, try)
import           Data.Maybe        (isJust)

-- | Whether an exception was delivered asynchronously (a cancellation or a thread kill)
-- rather than raised by the action itself. These must be re-thrown, never reported.
isAsync :: SomeException -> Bool
isAsync e = isJust (fromException e :: Maybe SomeAsyncException)

-- | 'try' for synchronous exceptions only. An asynchronous one is re-thrown rather than
-- captured as a result.
trySync :: IO a -> IO (Either SomeException a)
trySync act = do
  r <- try act
  case r of
    Left e | isAsync e -> throwIO e
    _                  -> pure r
