-- | The SQLite persistence effect: a connection pool plus free functions. There is no
-- swappable record here, because faking the database in tests works better against a real
-- temp DB. The schema is versioned through @PRAGMA user_version@. An observation stores its
-- rich 'Perception' as a JSON column, with the scalar metadata (time, camera, origin) in
-- queryable columns beside it.
--
-- This module is a re-export facade over the submodules ("PetReport.Effect.Db.Handle",
-- ".Migrations", ".Queries", ".Browse", ".Gc", ".Rollup"). Import it, not them.
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
  , overridesForObs
  , overridesForObsIds
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
  , revertObservation
  , editObservation
  , reprojectAll
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
  , BrowseQuery (..)
  , BrowsePage (..)
  , PetFilter (..)
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
                                                 SortDir (..), TimeBucket (..),
                                                 browseMoments, decodeCursor,
                                                 emptyBrowseQuery, encodeCursor)
import           PetReport.Effect.Db.Gc         (collectUnkept, countCollectable,
                                                 hasUnkeptBetween)
import           PetReport.Effect.Db.Handle     (Handle (..), withHandle)
import           PetReport.Effect.Db.Migrations (latestSchemaVersion)
import           PetReport.Effect.Db.Rollup     (dailyPetStats, materializeDay)
import           PetReport.Effect.Db.Queries    (Keepsake (..), PetSummary (..),
                                                 correctObservation,
                                                 correctionStats,
                                                 deleteKeepsake,
                                                 deleteObservation,
                                                 deleteObservations,
                                                 editObservation, eventStored,
                                                 getObservation,
                                                 getObservationsByIds,
                                                 getPetSummary,
                                                 getDaySwept,
                                                 getIngestDrained,
                                                 getIngestWatermark,
                                                 getProfile, getState,
                                                 insertKeepsake,
                                                 insertObservation,
                                                 insertReport,
                                                 keptObsIds, keptSampleStamps,
                                                 latestReport, listKeepsakes,
                                                 markReviewed,
                                                 obsEventId, observationsBetween,
                                                 overridesBetween,
                                                 overridesForObs,
                                                 overridesForObsIds,
                                                 purgePetAndProfile,
                                                 modifyProfile, modifyProfileE,
                                                 putPetSummary,
                                                 putProfile,
                                                 recentEventStarts,
                                                 reportExists, reprojectAll,
                                                 revertObservation, schemaVersion,
                                                 setDaySwept,
                                                 setIngestDrained,
                                                 setIngestWatermark,
                                                 setState, setTranscript,
                                                 subjectStatsBetween, transcriptsFor,
                                                 tsRange)
