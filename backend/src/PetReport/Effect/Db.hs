-- | The SQLite persistence effect: a connection pool plus free functions. There is no
-- swappable record here, because faking the database in tests works better against a real
-- temp DB. The schema is versioned through @PRAGMA user_version@. An observation stores its
-- rich 'Perception' as a JSON column, with the scalar metadata (time, camera, origin) in
-- queryable columns beside it.
--
-- This module is a re-export facade over the submodules ("PetReport.Effect.Db.Handle",
-- ".Migrations", ".Queries", ".Browse", ".Gc", ".Rollup", ".State", ".Keepsakes").
-- Import it, not
-- them.
module PetReport.Effect.Db
  ( Handle (..)
  , withHandle
  , getProfile
  , putProfile
  , modifyProfile
  , modifyProfileE
  , insertObservation
  , observationsBetween
  , getObservation
  , getObservationsByIds
  , overridesBetween
  , allAttributionsForObs
  , countedAttributionsForObsIds
  , subjectStatsBetween
  , materializeDay
  , dailyPetStats
  , collectUnkept
  , hasUnkeptBetween
  , countCollectable
  , markReviewed
  , deleteObservation
  , deleteObservations
  , purgePetAndProfile
  , getState
  , setState
  , getIngestWatermark
  , setIngestWatermark
  , getIngestDrained
  , setIngestDrained
  , getDaySwept
  , setDaySwept
  , insertReport
  , reportExists
  , latestReport
  , eventStored
  , recentEventStarts
  , correctObservation
  , addObservationSighting
  , removeObservationSighting
  , revertObservation
  , unreviewObservation
  , editObservation
  , AddOutcome (..)
  , maxSightingsPerMoment
  , reprojectAll
  , repairNamedSpecies
  , correctionStats
  , tsRange
  , obsEventId
  , setTranscript
  , transcriptsFor
  , Keepsake (..)
  , insertKeepsake
  , listKeepsakes
  , keptObsIds
  , keptSampleStamps
  , deleteKeepsake
  , PetSummary (..)
  , getPetSummary
  , putPetSummary
  , schemaVersion
  , latestSchemaVersion
    -- Faceted, cursor-paged browse
  , browseMoments
  , emptyBrowseQuery
  , escapeLike
  , BrowseQuery (..)
  , BrowsePage (..)
  , PetFilter (..)
  , SubjectFilter (..)
  , Behaviour (..)
  , ReviewFilter (..)
  , MediaKind (..)
  , TimeBucket (..)
  , SortDir (..)
  , Cursor (..)
  , encodeCursor
  , decodeCursor
  ) where

import           PetReport.Effect.Db.Browse     (BrowsePage (..), BrowseQuery (..),
                                                 Cursor (..), MediaKind (..),
                                                 PetFilter (..), ReviewFilter (..),
                                                 SubjectFilter (..), Behaviour (..),
                                                 SortDir (..), TimeBucket (..),
                                                 browseMoments, decodeCursor,
                                                 emptyBrowseQuery, escapeLike, encodeCursor)
import           PetReport.Effect.Db.Gc         (collectUnkept, countCollectable,
                                                 hasUnkeptBetween)
import           PetReport.Effect.Db.Handle     (Handle (..), withHandle)
import           PetReport.Effect.Db.Migrations (latestSchemaVersion)
import           PetReport.Effect.Db.Rollup     (dailyPetStats, materializeDay)
import           PetReport.Effect.Db.State      (getDaySwept, getIngestDrained,
                                                 getIngestWatermark, getState,
                                                 setDaySwept, setIngestDrained,
                                                 setIngestWatermark, setState)
import           PetReport.Effect.Db.Keepsakes  (Keepsake (..), PetSummary (..),
                                                 deleteKeepsake, getPetSummary,
                                                 insertKeepsake, keptObsIds,
                                                 keptSampleStamps, listKeepsakes,
                                                 putPetSummary)
import           PetReport.Effect.Db.Queries    (addObservationSighting,
                                                 correctObservation,
                                                 correctionStats,
                                                 deleteObservation,
                                                 deleteObservations,
                                                 editObservation, eventStored,
                                                 getObservation,
                                                 getObservationsByIds, getProfile,
                                                 insertObservation, insertReport,
                                                 latestReport, markReviewed,
                                                 modifyProfile, modifyProfileE,
                                                 obsEventId, observationsBetween,
                                                 overridesBetween,
                                                 allAttributionsForObs,
                                                 countedAttributionsForObsIds,
                                                 purgePetAndProfile, putProfile,
                                                 recentEventStarts, reportExists,
                                                 removeObservationSighting,
                                                 repairNamedSpecies,
                                                 reprojectAll, revertObservation,
                                                 unreviewObservation,
                                                 AddOutcome (..),
                                                 maxSightingsPerMoment,
                                                 schemaVersion, setTranscript,
                                                 subjectStatsBetween,
                                                 transcriptsFor, tsRange)
