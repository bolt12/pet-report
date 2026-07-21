-- | The pipeline's on-disk frame queue and proof-store plumbing: write a captured frame into
-- the per-camera queue, promote one to the proof store, plus the small read, list and remove
-- primitives the batch stage uses over those directories. Leaf file IO, with no App or
-- domain dependency, kept apart from the stage orchestration.
module PetReport.Pipeline.Queue
  ( writeQueueFrame
  , moveToProof
  , listJpgs
  , withTs
  , tryRead
  , removeQuiet
  ) where

import           Control.Exception (SomeException, try)
import qualified Data.ByteString   as BS
import           Data.Text         (Text)
import qualified Data.Text         as T
import           System.Directory  (createDirectoryIfMissing, doesDirectoryExist,
                                    listDirectory, removeFile, renameFile)
import           System.FilePath   (takeBaseName, takeExtension, (</>))
import           Text.Read         (readMaybe)

import           PetReport.Config  (Config (..))

-- | Write a captured JPEG into the per-camera queue directory, named by its POSIX
-- second, via a @.tmp@ rename so a reader never sees a half-written frame.
writeQueueFrame :: Config -> Text -> Integer -> BS.ByteString -> IO ()
writeQueueFrame cfg cam ts jpg = do
  let dir = cfgQueueDir cfg </> T.unpack cam
      path = dir </> (show ts <> ".jpg")
      tmp = path <> ".tmp"
  createDirectoryIfMissing True dir
  BS.writeFile tmp jpg
  renameFile tmp path

-- | Promote a queued frame to its camera's proof store by moving the file. That frame
-- becomes the moment's still.
moveToProof :: Config -> Text -> FilePath -> IO ()
moveToProof cfg cam fname = do
  let src = cfgQueueDir cfg </> T.unpack cam </> fname
      ddir = cfgProofDir cfg </> T.unpack cam
  createDirectoryIfMissing True ddir
  renameFile src (ddir </> fname)

-- | The @.jpg@ files in a directory, or nothing when it does not exist.
listJpgs :: FilePath -> IO [FilePath]
listJpgs dir = do
  ok <- doesDirectoryExist dir
  if ok
    then filter ((== ".jpg") . takeExtension) <$> listDirectory dir
    else pure []

-- | Pair a queue filename with the POSIX second parsed from its base name, or
-- 'Nothing' if the name is not a timestamp.
withTs :: FilePath -> Maybe (FilePath, Integer)
withTs f = (,) f <$> readMaybe (takeBaseName f)

-- | Read a file, or 'Nothing' if it is gone or unreadable (a queue frame may be
-- pruned between listing and reading).
tryRead :: FilePath -> IO (Maybe BS.ByteString)
tryRead path = do
  r <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  pure (either (const Nothing) Just r)

-- | Remove a file, ignoring any failure (already gone is fine).
removeQuiet :: FilePath -> IO ()
removeQuiet path = do
  _ <- try (removeFile path) :: IO (Either SomeException ())
  pure ()
