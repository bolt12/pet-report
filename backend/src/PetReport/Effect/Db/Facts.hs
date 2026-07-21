-- | The facts projection (@observation_subjects@) writer and the derived per-observation
-- signals stored beside it. Every perception write in "PetReport.Effect.Db.Queries" (insert,
-- correction, edit, reproject) funnels through 'writeFacts', so no two writers can project
-- the same perception differently.
module PetReport.Effect.Db.Facts
  ( confOf
  , decodePerception
  , writeFacts
  , boolToInt
  ) where

import           Control.Monad                  (forM_)
import           Data.Aeson                     (eitherDecodeStrict)
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import           Data.Text.Encoding             (encodeUtf8)
import           Database.SQLite.Simple         (Connection, Only (..), SQLData,
                                                 execute)
import           Database.SQLite.Simple.ToField (toField)

import           PetReport.Domain.Perception    (Perception)
import           PetReport.Domain.Stats         (SubjectFact (..), factsOf)

-- | Decode a stored perception blob back into a 'Perception'. Every reader of the
-- @perception@ column comes through here, so the backfill, the row decode and the
-- reprojection share one decoder and one error convention.
decodePerception :: Text -> Either String Perception
decodePerception = eitherDecodeStrict . encodeUtf8

-- | The value stored in @observations.confidence@: the scene confidence 'factsOf' stamps on
-- every projected fact, or 'Nothing' when there are none (a sound, or a scene with no
-- appearances).
confOf :: Perception -> Maybe Double
confOf p = case factsOf p of
  (f : _) -> sfConfidence f
  []      -> Nothing

-- | Rewrite an observation's projected subject rows from its perception. Runs on insert,
-- where there is nothing to delete, and after a correction or edit. Identity stays derived:
-- species is stored, and resolves to a pet at query time.
writeFacts :: Connection -> Int64 -> Perception -> IO ()
writeFacts c oid p = do
  execute c "DELETE FROM observation_subjects WHERE obs_id = ?" (Only oid)
  forM_ (zip [0 ..] (factsOf p)) $ \(i, f) ->
    execute
      c
      "INSERT INTO observation_subjects \
      \(obs_id, seq, species, is_person, activity, rest, active, ate, drank, slept, played, groomed, \
      \eliminated, accident, concern, confidence) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      (subjectRow oid i f)

-- | The bind list for one @observation_subjects@ row, in exactly the column order of
-- the INSERT in 'writeFacts'.
subjectRow :: Int64 -> Int -> SubjectFact -> [SQLData]
subjectRow oid s f =
  [ toField oid
  , toField s
  , toField (sfSpecies f)
  , toField (bi (sfIsPerson f))
  , toField (sfActivity f)
  , toField (bi (sfRest f))
  , toField (bi (sfActive f))
  , toField (bi (sfAte f))
  , toField (bi (sfDrank f))
  , toField (bi (sfSlept f))
  , toField (bi (sfPlayed f))
  , toField (bi (sfGroomed f))
  , toField (bi (sfEliminated f))
  , toField (bi (sfAccident f))
  , toField (bi (sfConcern f))
  , toField (sfConfidence f)
  ]
  where
    bi = boolToInt

-- | A boolean as SQLite's 0/1 integer, shared by the facts projection and the
-- @observations.uncertain@ writers so both agree on the encoding.
boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0
