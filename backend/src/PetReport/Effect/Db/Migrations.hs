{-# LANGUAGE TemplateHaskell #-}

-- | The versioned schema: the ordered @migrations@ list, applied by @migrate@ at open time,
-- each in its own transaction and gated on @PRAGMA user_version@. There is one entry so far,
-- the initial schema; a future change appends version 2, then 3, and so on.
--
-- Kept apart from "PetReport.Effect.Db.Queries" so the handle can run migrations without
-- pulling in every query.
module PetReport.Effect.Db.Migrations
  ( migrate
  , currentVersion
  , latestSchemaVersion
  , schemaTooNew
  , unversionedButPopulated
  , schemaV1Tables
  , migrations
  ) where

import           Control.Monad          (forM_, when)
import           Data.FileEmbed         (embedStringFile, makeRelativeToProject)
import           Data.List              (sort)
import           Data.Maybe             (listToMaybe, mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, Query (..), execute_,
                                         fromOnly, query_,
                                         withImmediateTransaction)

import           PetReport.Trace (DbEvent (..), Tracer, traceWith)

-- | Bring the database up to 'latestSchemaVersion': refuse a schema newer than this
-- build understands ('schemaTooNew'), then apply each pending 'migrations' entry in
-- its own transaction. Run by 'PetReport.Effect.Db.Handle.withHandle' at open time,
-- on a dedicated connection before the pool exists.
migrate :: Tracer IO DbEvent -> Connection -> IO ()
migrate tracer c = do
  cur <- currentVersion c
  -- Refuse a database whose schema is NEWER than this build understands. The runner would
  -- apply no migration, yet the code would query columns that schema lacks, and every
  -- request would hit an opaque SQL error. Fail fast with something actionable, so the
  -- operator reads it once at startup rather than as a 500 per request.
  refuse SchemaTooNew (schemaTooNew cur)
  -- Version 0 usually means a brand-new file, but SQLite reports a never-stamped database
  -- the same way, so the number alone cannot separate them. Applying version 1 to an
  -- already-populated file dies on its first CREATE TABLE with a raw SQLite error naming a
  -- table the operator never asked about. Only look inside when the stamp is 0, which
  -- keeps this off the common startup path.
  when (cur == 0) $
    refuse SchemaUnversioned . unversionedButPopulated =<< userTables c
  forM_ migrations $ \(v, stmts) ->
    when (cur < v) $ do
      traceWith tracer (ApplyingMigration v)
      -- SQLite DDL and user_version are both transactional, so each version applies
      -- atomically and a crash mid-migration cannot leave a half-applied version with the
      -- number unbumped. IMMEDIATE matches Queries' BEGIN IMMEDIATE; there is no
      -- concurrency here yet, but keeping every write transaction consistent is cheaper
      -- than remembering which ones are special.
      withImmediateTransaction c $ do
        mapM_ (execute_ c) stmts
        execute_ c (Query ("PRAGMA user_version = " <> T.pack (show v)))
  traceWith tracer $
    if cur < latestSchemaVersion
      then SchemaMigrated latestSchemaVersion
      else SchemaUpToDate cur
  where
    -- Report an actionable refusal through the tracer, then abort startup with the same
    -- text. Every open-time guard goes through here, so none can trace without dying or
    -- die without telling the operator why.
    refuse :: (Text -> DbEvent) -> Maybe String -> IO ()
    refuse mkEvent = mapM_ $ \msg ->
      traceWith tracer (mkEvent (T.pack msg)) >> ioError (userError msg)

-- | The schema version the database sits at, read from @PRAGMA user_version@ (0 for
-- a fresh file).
currentVersion :: Connection -> IO Int
currentVersion c = do
  rows <- query_ c "PRAGMA user_version"
  pure (maybe 0 fromOnly (listToMaybe rows))

-- | The schema version a fully-migrated DB sits at (the last entry in 'migrations').
latestSchemaVersion :: Int
latestSchemaVersion = maximum (0 : map fst migrations)

-- | 'Nothing' when a database at schema version @cur@ can be opened by this build, 'Just'
-- an actionable message when it is newer. In that case 'migrate' would apply nothing, yet
-- the code would go on to query columns this build's schema does not have.
schemaTooNew :: Int -> Maybe String
schemaTooNew cur
  | cur > latestSchemaVersion =
      Just $
        "database schema version "
          <> show cur
          <> " is newer than this build supports (version "
          <> show latestSchemaVersion
          <> "). It was written by a later version of pet-report. Upgrade, or back up and \
             \remove the database file and restart to recreate it."
  | otherwise = Nothing

-- | The tables the database already holds, excluding SQLite's own @sqlite_%@ bookkeeping
-- objects. Only used to tell a fresh file from an unstamped one.
userTables :: Connection -> IO [Text]
userTables c =
  map fromOnly
    <$> query_
      c
      "SELECT name FROM sqlite_master \
      \WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name"

-- | 'Nothing' when a database reporting version 0 is genuinely empty and the initial schema
-- can be applied. 'Just' an actionable message when it already holds @tables@, since then it
-- is indistinguishable from a stamped database that lost its stamp. Split out from 'migrate'
-- so the decision is testable without a live 'Connection', as with 'schemaTooNew'.
--
-- The advice here differs from 'schemaTooNew'\'s on purpose. A too-new database really does
-- have to go, but this state is reached by a @sqlite3 .dump@ and reimport, which does not
-- carry @PRAGMA user_version@ across (@VACUUM@ does). A restored backup of a perfectly
-- healthy database lands here, so "remove the file" would be advice to delete live data.
-- Lead with restoring the stamp, and only suggest moving the file aside when it turns out
-- not to be a pet-report database at all.
unversionedButPopulated :: [Text] -> Maybe String
unversionedButPopulated [] = Nothing
unversionedButPopulated tables
  | tables == schemaV1Tables =
      Just $
        "database holds exactly the tables this build's schema creates but carries no \
        \schema version, so it is almost certainly a pet-report database that lost its \
        \stamp. Restoring a backup through sqlite3's .dump and .read does that (VACUUM \
        \does not). Put the stamp back with \"PRAGMA user_version = "
          <> show latestSchemaVersion
          <> "\" and restart. Do not delete the file: the data is intact."
  | otherwise =
      Just $
        "database holds tables ("
          <> T.unpack (T.intercalate ", " tables)
          <> ") but carries no schema version, and they are not this build's schema ("
          <> T.unpack (T.intercalate ", " schemaV1Tables)
          <> "), so it cannot be adopted. Move the file aside and restart to recreate it."

-- | The tables the initial schema creates, recovered from the same embedded DDL
-- 'migrations' applies, so the list updates itself when the schema changes. Sorted to match
-- 'userTables', which reads them back @ORDER BY name@. The parsing is as simple as
-- 'parseStatements', and can afford to be: our own DDL says @CREATE TABLE <name> (@ and
-- nothing else.
schemaV1Tables :: [Text]
schemaV1Tables = sort (mapMaybe tableName (parseStatements schemaV1))
  where
    tableName (Query q) = case T.words (T.replace "(" " ( " q) of
      a : b : name : _ | T.toUpper a == "CREATE", T.toUpper b == "TABLE" -> Just name
      _                                                                  -> Nothing

migrations :: [(Int, [Query])]
migrations = [(1, parseStatements schemaV1)]

-- | The initial schema, embedded from @sql/0001_initial.sql@ at compile time. The DDL lives
-- in a diffable, syntax-highlighted @.sql@ file while the binary stays self-contained, with
-- no runtime data-file dependency. A future version adds @sql/0002_*.sql@ and a @(2, ...)@
-- entry to 'migrations'.
schemaV1 :: Text
schemaV1 = $(makeRelativeToProject "sql/0001_initial.sql" >>= embedStringFile)

-- | Split a @.sql@ script into its statements. Whole-line @--@ comments go FIRST, so a @;@
-- inside a comment cannot end a statement; then split on @;@, trim, and drop empty chunks.
-- This stays simple because the schema DDL has no @;@ inside a string literal, so it
-- recovers exactly the intended statements.
parseStatements :: Text -> [Query]
parseStatements =
  map Query
    . filter (not . T.null)
    . map T.strip
    . T.splitOn ";"
    . stripLineComments
  where
    stripLineComments = T.unlines . filter (not . isLineComment) . T.lines
    isLineComment = T.isPrefixOf "--" . T.stripStart
