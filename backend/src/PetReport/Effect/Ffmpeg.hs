-- | Frame extraction from a Frigate clip, as a record-of-functions handle. Best-effort:
-- interior frames are sampled evenly, avoiding the black first and last frame, and a frame
-- that cannot be grabbed is dropped.
--
-- This shells out to @ffprobe@ and @ffmpeg@ through typed-process rather than binding
-- something like @ffmpeg-light@, which would add a native libav link dependency and hand
-- back decoded pixel buffers we would only re-encode to the JPEG the vision model wants.
-- The binaries just need to be on @PATH@.
module PetReport.Effect.Ffmpeg
  ( Handle (..)
  , withHandle
  ) where

import           Control.Exception    (SomeException, try)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           System.Exit          (ExitCode (ExitSuccess))
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)
import           System.Process.Typed (proc, readProcess, runProcess)
import           Text.Read            (readMaybe)

import           PetReport.Util (catchSync)

newtype Handle = Handle
  { clipFrames :: BS.ByteString -> Int -> IO [BS.ByteString]
  -- ^ @clipFrames clipBytes n@ extracts up to @n@ interior JPEG frames.
  }

withHandle :: (Handle -> IO a) -> IO a
withHandle k = k Handle {clipFrames = extractFrames}

extractFrames :: BS.ByteString -> Int -> IO [BS.ByteString]
extractFrames clip n
  | n <= 0 = pure []
  -- A synchronous failure (a bad clip, a missing ffmpeg) degrades to no frames.
  -- 'catchSync' handles only synchronous exceptions, so a shutdown cancellation
  -- mid-extraction still propagates instead of being swallowed.
  | otherwise = withSystemTempDirectory "petreport-clip" go `catchSync` \_ -> pure []
  where
    go dir = do
      let clipPath = dir </> "clip.mp4"
      BS.writeFile clipPath clip
      mdur <- probeDuration clipPath
      case mdur of
        Nothing -> pure []
        Just dur -> do
          let stamps =
                [dur * fromIntegral i / fromIntegral (n + 1) | i <- [1 .. n]]
          catMaybes <$> mapM (grabFrame dir clipPath) (zip [1 :: Int ..] stamps)

probeDuration :: FilePath -> IO (Maybe Double)
probeDuration path = do
  (_, out, _) <-
    readProcess
      ( proc
          "ffprobe"
          [ "-v", "error"
          , "-show_entries", "format=duration"
          , "-of", "default=nw=1:nk=1"
          , path
          ]
      )
  pure (readMaybe (T.unpack (T.strip (TE.decodeUtf8Lenient (LBS.toStrict out)))))

grabFrame :: FilePath -> FilePath -> (Int, Double) -> IO (Maybe BS.ByteString)
grabFrame dir clipPath (i, ts) = do
  let outPath = dir </> ("frame" <> show i <> ".jpg")
  ec <-
    runProcess
      ( proc
          "ffmpeg"
          [ "-nostdin", "-loglevel", "error", "-y"
          , "-ss", show ts
          , "-i", clipPath
          , "-frames:v", "1"
          , "-q:v", "3"
          , outPath
          ]
      )
  case ec of
    -- On a non-zero exit ffmpeg may have left a truncated JPEG behind. Don't read it; a
    -- partial frame must never reach the vision model.
    ExitSuccess -> do
      r <- try (BS.readFile outPath) :: IO (Either SomeException BS.ByteString)
      pure (either (const Nothing) Just r)
    _ -> pure Nothing
