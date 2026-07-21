-- | The database handle: a pool of SQLite connections plus a tracer. Each concurrent user,
-- request handlers and background loops alike, borrows its own connection, so WAL readers
-- run in parallel while SQLite's single writer and its busy_timeout serialise the writes.
-- The pool is not what provides consistency.
--
-- 'withHandle' opens the pool, runs the one-time setup, and tears it down. Migrations run
-- before the pool exists, on a dedicated connection. The record fields are exported so the
-- sibling query modules can reach them.
module PetReport.Effect.Db.Handle
  ( Handle (..)
  , withConn
  , withHandle
  ) where

import           Control.Exception              (bracket)
import           Data.Pool                      (Pool, defaultPoolConfig,
                                                 destroyAllResources, newPool,
                                                 setNumStripes, withResource)
import           Database.SQLite.Simple         (Connection, close, execute_,
                                                 open)
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                (takeDirectory)

import           PetReport.Effect.Db.Migrations (migrate)
import           PetReport.Trace                (DbEvent, Tracer)

data Handle = Handle
  { pool    :: Pool Connection
  , hTracer :: Tracer IO DbEvent
  }

-- | Borrow a pooled connection for the duration of an action. Every query opens this way,
-- which keeps @pool@ an implementation detail rather than a phrase repeated at each call
-- site.
withConn :: Handle -> (Connection -> IO a) -> IO a
withConn h = withResource (pool h)

-- | Open the pool, running one-time setup and migrations, and tear it down after.
withHandle :: Tracer IO DbEvent -> FilePath -> (Handle -> IO a) -> IO a
withHandle tracer path k = bracket acquire destroyAllResources (\p -> k (Handle p tracer))
  where
    acquire = do
      createDirectoryIfMissing True (takeDirectory path)
      bracket (open path) close (\c -> setupConn c >> migrate tracer c)
      -- One stripe. SQLite has a single writer, and the default of one stripe per
      -- capability would exceed maxResources under the executable's @-N@ RTS.
      newPool (setNumStripes (Just 1) (defaultPoolConfig openReady close 60 4))
    openReady = do
      c <- open path
      setupConn c
      pure c

-- | Pragmas applied to every pooled connection. @busy_timeout@ and @foreign_keys@ are
-- connection-local, so each fresh connection has to set them. @journal_mode=WAL@ is a
-- persistent property of the file and re-runs harmlessly.
setupConn :: Connection -> IO ()
setupConn c = do
  execute_ c "PRAGMA journal_mode=WAL"
  execute_ c "PRAGMA busy_timeout=15000"
  execute_ c "PRAGMA foreign_keys=ON"
