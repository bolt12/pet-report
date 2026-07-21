-- | Small shared helpers with no home of their own: JSON field-name derivation, a few text
-- utilities, and the sync/async exception split. The codec-derived schema helper lives in
-- "LLM.Schema" instead, since that belongs to the app-agnostic harness.
module PetReport.Util
  ( -- * JSON
    prefixed
    -- * Text
  , tshow
  , capitalize
  , nonBlank
  , boundedLines
  , paragraphs
    -- * Time
  , microseconds
    -- * Exceptions
  , isAsyncException
  , catchSync
  , trySync
  ) where

import           Control.Exception (SomeAsyncException, SomeException, catch,
                                    fromException, throwIO)
import           Data.Aeson        (Options (..), defaultOptions)
import           Data.Char         (toLower, toUpper)
import           Data.Maybe        (isJust)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Time.Clock   (NominalDiffTime)

-- --------------------------------------------------------------------------- --
-- JSON
-- --------------------------------------------------------------------------- --

-- | JSON 'Options' that strip an @n@-character field-name prefix and lower-case
-- the first remaining letter, so a record field @ovSubjectLabel@ serialises to
-- @subjectLabel@. Used by the view/DTO types that prefix their fields to avoid
-- duplicate-record-field clashes with the domain types.
prefixed :: Int -> Options
prefixed n = defaultOptions {fieldLabelModifier = lower1 . drop n}
  where
    lower1 (c : cs) = toLower c : cs
    lower1 []       = []

-- --------------------------------------------------------------------------- --
-- Text
-- --------------------------------------------------------------------------- --

-- | @Data.Text.pack . show@.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Upper-case the first character; empty text is unchanged.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t

-- | The stripped text, or 'Nothing' when it is empty or all whitespace. Both the
-- model-output and env decoders treat blank as absent through here.
nonBlank :: Text -> Maybe Text
nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s

-- | Join prompt lines with newlines, dropping blank ones so an absent optional section
-- leaves no stray newline. Every prompt builder assembles through here.
paragraphs :: [Text] -> Text
paragraphs = T.intercalate "\n" . filter (not . T.null)

-- | Join lines with newlines, keeping at most the most recent @n@ so a very busy
-- window cannot overflow the model's context. When trimmed, a leading note
-- records how many earlier lines were dropped.
boundedLines :: Int -> [Text] -> Text
boundedLines n ls
  | len <= n = T.intercalate "\n" ls
  | otherwise =
      T.intercalate "\n" $
        ("(" <> tshow (len - n) <> " earlier lines omitted)") : drop (len - n) ls
  where
    len = length ls

-- --------------------------------------------------------------------------- --
-- Time
-- --------------------------------------------------------------------------- --

-- | A 'NominalDiffTime' as whole microseconds, for the seams where a duration meets
-- 'Control.Concurrent.threadDelay' and req's microsecond timeouts.
microseconds :: NominalDiffTime -> Int
microseconds d = round (realToFrac d * 1e6 :: Double)

-- --------------------------------------------------------------------------- --
-- Exceptions
-- --------------------------------------------------------------------------- --

-- | Whether an exception is asynchronous (a cancellation or thread kill), and so
-- must be re-thrown rather than handled. Every async exception wraps in
-- 'SomeAsyncException' via @asyncExceptionToException@, so this one check covers
-- 'Control.Concurrent.Async.AsyncCancelled', 'ThreadKilled', and the rest.
isAsyncException :: SomeException -> Bool
isAsyncException e = isJust (fromException e :: Maybe SomeAsyncException)

-- | Run an action, handling only SYNCHRONOUS exceptions; an asynchronous one
-- (shutdown cancellation, thread kill) is re-thrown so it still propagates.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync act handler =
  act `catch` \e -> if isAsyncException e then throwIO e else handler e

-- | Like 'Control.Exception.try' but only for synchronous exceptions: an asynchronous
-- one (shutdown cancellation, thread kill) still propagates. The 'Either' companion to
-- 'catchSync'.
trySync :: IO a -> IO (Either SomeException a)
trySync act = fmap Right act `catchSync` (pure . Left)
