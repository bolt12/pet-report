-- | The web layer's on-disk media store and the path validators guarding it: owned copies
-- of a kept moment's Frigate media, the periodic-sample proof frames, and the
-- @[A-Za-z0-9._-]@ shape checks that stop a camera name or event id escaping its directory.
--
-- Pure file IO with no Servant dependency, kept out of the handler module so the HTTP
-- surface reads as handlers over this store.
module PetReport.Web.Media
  ( saveOwnedMedia
  , removeOwnedMedia
  , eventMediaBytes
  , readProof
  , removeProofFor
  , decodePhoto
  , resolvePetPhoto
  , petPhotoBytes
  , removePetPhoto
  , validCam
  , validEventId
  , validProofFile
  ) where

import           Control.Exception            (SomeException, try)
import           Control.Monad                (void)
import           Data.Bits                    (xor)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as B64
import           Data.Char                    (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)
import           Data.Word                    (Word32, Word8)
import           Numeric                      (showHex)
import           System.Directory             (createDirectoryIfMissing,
                                               removeFile, renameFile)
import           System.FilePath              ((</>))

import           PetReport.Config             (Config (..))
import           PetReport.Domain.Observation (FrigateMeta (..), Observation (..),
                                               Origin (..))
import           PetReport.Domain.Types       (EventId (..), cameraText)

-- | The on-disk path of a kept event's owned media copy, keyed by Frigate event id.
-- 'validEventId' has already excluded a separator, so the id cannot escape the media
-- directory.
ownedMediaPath :: Config -> Text -> Text -> FilePath
ownedMediaPath cfg eid ext = cfgMediaDir cfg </> (T.unpack eid <> T.unpack ext)

-- | Write an event's owned media copy atomically, temp then rename. A 'Nothing' payload
-- means Frigate no longer had the media, and is a quiet no-op: keeping is best-effort.
saveOwnedMedia :: Config -> Text -> Text -> Maybe ByteString -> IO ()
saveOwnedMedia _ _ _ Nothing = pure ()
saveOwnedMedia cfg eid ext (Just bs) = do
  let path = ownedMediaPath cfg eid ext
  BS.writeFile (path <> ".tmp") bs
  renameFile (path <> ".tmp") path

-- | Delete both owned media files for an event, quietly.
removeOwnedMedia :: Config -> FrigateMeta -> IO ()
removeOwnedMedia cfg m =
  let EventId eid = eventId m
   in mapM_
        (\ext -> void (try (removeFile (ownedMediaPath cfg eid ext)) :: IO (Either SomeException ())))
        [".jpg", ".mp4"]

-- | Read a file's bytes, or 'Nothing' if it is absent or unreadable. The whole store turns
-- "no such file" into a 'Nothing' rather than an exception here.
tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile path = do
  r <- try (BS.readFile path) :: IO (Either SomeException ByteString)
  pure (either (const Nothing) Just r)

readOwnedMedia :: Config -> Text -> Text -> IO (Maybe ByteString)
readOwnedMedia cfg eid ext = tryReadFile (ownedMediaPath cfg eid ext)

-- | An event's media bytes: the owned copy if this moment was kept, otherwise fetched live
-- from Frigate through the supplied fetcher.
eventMediaBytes :: Config -> Text -> Text -> (Text -> IO (Maybe ByteString)) -> IO (Maybe ByteString)
eventMediaBytes cfg eid ext fetch = do
  owned <- readOwnedMedia cfg eid ext
  maybe (fetch eid) (pure . Just) owned

-- | Read a proof frame's bytes by camera and filename. This does no validation itself, so
-- callers MUST have checked 'validCam' and 'validProofFile' first; the raw segments could
-- otherwise escape the proof directory.
readProof :: Config -> Text -> Text -> IO (Maybe ByteString)
readProof cfg cam file = tryReadFile (cfgProofDir cfg </> T.unpack cam </> T.unpack file)

-- | Free a deleted moment's on-disk media: a periodic sample's proof frame, or a kept
-- event's owned still and clip. Called after the row is gone, disk not being transactional,
-- so a deleted moment leaves nothing behind.
removeProofFor :: Config -> Observation -> IO ()
removeProofFor cfg obs = case origin obs of
  PeriodicSample -> do
    let ts = round (utcTimeToPOSIXSeconds (at obs)) :: Integer
        path = cfgProofDir cfg </> T.unpack (cameraText (camera obs)) </> (show ts <> ".jpg")
    _ <- try (removeFile path) :: IO (Either SomeException ())
    pure ()
  FromEvent m -> removeOwnedMedia cfg m

-- | The on-disk path of a pet's avatar photo, keyed by pet id. 'validPetId' has
-- already excluded a separator, so the id cannot escape the media directory.
petPhotoPath :: Config -> Text -> FilePath
petPhotoPath cfg pid = cfgMediaDir cfg </> ("pet-" <> T.unpack pid <> ".jpg")

-- | The largest photo we accept, a generous cap over the client's ~1024px downscale
-- so a hand-crafted request cannot force a huge decode or disk write.
maxPhotoBytes :: Int
maxPhotoBytes = 8 * 1024 * 1024

-- | Decode a raw base64 image payload (the client strips the @data:@ URL prefix) to
-- bytes, or 'Nothing' if it is missing, larger than 'maxPhotoBytes', or not valid
-- base64. Shared by the store path and the describe endpoint so a photo is validated
-- and decoded one way.
decodePhoto :: Text -> Maybe ByteString
decodePhoto raw
  | T.null stripped = Nothing
  | T.length stripped > b64Cap = Nothing
  | otherwise = case B64.decode (TE.encodeUtf8 stripped) of
      Right bytes | not (BS.null bytes) -> Just bytes
      _ -> Nothing
  where
    stripped = T.strip raw
    -- base64 encodes 3 bytes per 4 chars, so bound the encoded length to cap decode.
    b64Cap = (maxPhotoBytes `div` 3 + 1) * 4

-- | Decode a raw base64 image and store it as the pet's avatar, atomically (temp
-- then rename). Returns a short content token to record on the 'Pet' (which both
-- signals presence and cache-busts the served URL), or 'Nothing' when the id is
-- unsafe, the payload is undecodable, or the write fails. Best-effort and total: a
-- photo problem must never fail a save.
storePetPhoto :: Config -> Text -> Text -> IO (Maybe Text)
storePetPhoto cfg pid b64
  | not (validPetId pid) = pure Nothing
  | otherwise = case decodePhoto b64 of
      Nothing -> pure Nothing
      Just bytes -> do
        let path = petPhotoPath cfg pid
        r <-
          try $ do
            createDirectoryIfMissing True (cfgMediaDir cfg)
            BS.writeFile (path <> ".tmp") bytes
            renameFile (path <> ".tmp") path
        pure (either (const Nothing) (const (Just (photoToken bytes))) (r :: Either SomeException ()))

-- | Read a pet's stored avatar bytes, or 'Nothing' if the id is unsafe or absent.
petPhotoBytes :: Config -> Text -> IO (Maybe ByteString)
petPhotoBytes cfg pid
  | not (validPetId pid) = pure Nothing
  | otherwise = tryReadFile (petPhotoPath cfg pid)

-- | Delete a pet's avatar photo, quietly. Called on a hard delete (disk is not
-- transactional), so a removed pet leaves no photo behind.
removePetPhoto :: Config -> Text -> IO ()
removePetPhoto cfg pid
  | not (validPetId pid) = pure ()
  | otherwise = void (try (removeFile (petPhotoPath cfg pid)) :: IO (Either SomeException ()))

-- | Resolve a pet's avatar on an add or edit, returning the new token to record: an
-- explicit @remove@ clears it; a new base64 @mNew@ stores and replaces it (keeping
-- @existing@ if the write fails); otherwise @existing@ is kept untouched. Disk-only
-- and shared by the add and edit handlers, which each fold the token into their own
-- profile write, so the store/replace/remove decision lives in one place.
resolvePetPhoto :: Config -> Text -> Bool -> Maybe Text -> Maybe Text -> IO (Maybe Text)
resolvePetPhoto cfg pid remove mNew existing
  | remove = removePetPhoto cfg pid >> pure Nothing
  | otherwise = case mNew of
      Nothing  -> pure existing
      Just b64 -> maybe existing Just <$> storePetPhoto cfg pid b64

-- | A short, stable content token (FNV-1a, 32-bit, hex) over the stored bytes.
-- Deterministic and dependency-free: it only has to change when the image does, so
-- the @?v=@ cache-buster forces a re-fetch on a new photo.
photoToken :: ByteString -> Text
photoToken bs = T.pack (showHex (BS.foldl' step 2166136261 bs) "")
  where
    step :: Word32 -> Word8 -> Word32
    step h b = (h `xor` fromIntegral b) * 16777619

-- | The shape of a Frigate camera name the proof/frame path guards accept: letters
-- (either case), digits, underscore, and hyphen, as Frigate itself allows. Rejects
-- '/' and '.' so the name cannot escape the proof directory.
validCam :: Text -> Bool
validCam c = not (T.null c) && T.all ok c
  where
    ok x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_' || x == '-'

-- | The shape of a pet id the avatar path guard accepts: letters, digits,
-- underscore, and hyphen (the client generates slug ids). Rejects '/' and '.' so
-- the id cannot escape the media directory. Same character class as 'validCam'.
validPetId :: Text -> Bool
validPetId = validCam

-- | The @[A-Za-z0-9._-]+@ shape of a Frigate event id.
validEventId :: Text -> Bool
validEventId e = not (T.null e) && T.all ok e
  where
    ok c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '.' || c == '-' || c == '_'

-- | A proof-frame filename: @<posix-seconds>.jpg@, digits only, so it cannot escape
-- the camera's proof directory.
validProofFile :: Text -> Bool
validProofFile f = case T.stripSuffix ".jpg" f of
  Just base -> not (T.null base) && T.all isDigit base
  Nothing   -> False
