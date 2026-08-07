module Main
  ( main
  ) where

import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar  (newEmptyMVar, takeMVar,
                                           tryPutMVar)
import           Control.Monad            (void, (>=>))
import           Data.Aeson               (FromJSON, ToJSON, Value (..),
                                           decode, encode)
import qualified Data.Aeson.Key           as Key
import qualified Data.Aeson.KeyMap        as KeyMap
import qualified Data.ByteString.Lazy     as LBS
import           Data.Either              (isLeft)
import           Data.List                (sort)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (isJust, listToMaybe, mapMaybe)
import           Data.Text                (Text, isInfixOf, pack)
import           Data.Time                (Day, UTCTime (..), addDays,
                                           addUTCTime, fromGregorian)
import           Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)
import           Data.Time.Zones          (utcTZ)
import           Hedgehog                 (Gen, Property, assert, evalIO,
                                           failure, forAll, property,
                                           tripping, (===))
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Test.Tasty               (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog      (testProperty)
import           Test.Tasty.HUnit         (assertBool, assertFailure, testCase,
                                           (@?=))

import           System.FilePath ((</>))
import           System.IO.Temp  (withSystemTempDirectory)

import           Effects (effectGroups)

import           PetReport.Config               (mkHour)
import           PetReport.Domain.Behavior
import           PetReport.Pipeline.Scheduler   (nextBatchTime)
import           PetReport.Domain.Observation   (FrigateMeta (..),
                                                 NewObservation (..),
                                                 Observation (..), Origin (..))
import qualified PetReport.Effect.Db            as Db
import qualified PetReport.Effect.Db.Migrations as Migrations
import           PetReport.Trace                (Severity (..), dbTracer,
                                                 pipelineTracer, renderingTracer,
                                                 resolveLogLevel, sevText,
                                                 severityFromText)
import           PetReport.Domain.Perception
import           PetReport.Domain.PetReport     (KeepsakeV (..), RecapV (..),
                                                 StatPair (..),
                                                 WellbeingKind (..), hourHistogram,
                                                 insightsFor, petStatFor, petStatOver,
                                                 roomDistribution, wellbeingKindFromText,
                                                 wellbeingKindText)
import           PetReport.Domain.Profile
import           PetReport.Domain.Stats
import           PetReport.Domain.Types
import           PetReport.Domain.View          (Chip (..), ObsMedia (..),
                                                 ObsView (..), chipsOf,
                                                 fallbackDescription, mediaFor,
                                                 momentExpiry, viewOf)
import           PetReport.View.Enrich          (mkKeepsake, mkRecap,
                                                 wellbeingLine)
import           PetReport.Domain.Window
import           PetReport.Analysis.Vocabulary  (householdVisionNote)
import           PetReport.Util                 (boundedLines)
import           PetReport.Pipeline             (RunOutcome (..), advanceWatermark,
                                                 runOutcomeText)
-- Import constructors only (no field selectors) to avoid clashing with
-- PetReport.Domain.Stats.presence which is also in scope.
import           PetReport.Web             (AskResp (AskResp), CameraInfo (CameraInfo),
                                            DayResponse (DayResponse),
                                            PresenceV (PresenceV), resolveRefresh)
import qualified PetReport.Pipeline.Worker as Worker

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "pet-report" $
    [ testGroup
        "json round-trips"
        [ testProperty "Activity" (enumTrip (Gen.enumBounded :: Gen Activity))
        , testProperty "Wellbeing" (enumTrip (Gen.enumBounded :: Gen Wellbeing))
        , testProperty "ReportTopic" (enumTrip (Gen.enumBounded :: Gen ReportTopic))
        , testProperty "EliminationType" (enumTrip (Gen.enumBounded :: Gen EliminationType))
        , testProperty "EliminationPlace" (enumTrip (Gen.enumBounded :: Gen EliminationPlace))
        , testProperty "Appearance" (enumTrip genAppearance)
        , testProperty "Scene" (enumTrip genScene)
        , testProperty "Perception" (enumTrip genPerception)
        ]
    , testProperty "confidence is clamped to [0,1]" prop_confidenceClamped
    , testGroup "properties" props
    , testGroup "domain logic" domainUnits
    , testGroup "view + insights" viewUnits
    , testGroup "persistence" dbUnits
    , testGroup "api contract" contractUnits
    , testGroup "wire vocabularies" vocabularyUnits
    , testGroup "worker" workerUnits
    , testGroup "ingest watermark" ingestUnits
    , testGroup "perception invariants" perceptionUnits
    , testGroup "scheduler" schedulerUnits
    ]
      ++ effectGroups

-- The pure watermark-advance rule, which is what keeps ingest contiguous.
ingestUnits :: [TestTree]
ingestUnits =
  [ testProperty "a full page or a frozen fold holds strictly below the fold max (tie-safe)" prop_advanceTieSafe
  , testProperty "the frozen/full-page nudge never drifts below the window's lo" prop_advanceClampsLo
  , testCase "a drained short page advances to hi (steps past a quiet gap)" $ do
      advanceWatermark 50 False 0 100 200 @?= 200 -- no events in the window: jump to hi
      advanceWatermark 50 False 3 150 200 @?= 200 -- drained under the limit: to hi
  , testCase "a freeze-at-start pass (foldMax == lo) holds exactly lo, not lo - eps" $
      -- The fold froze before advancing past any event, so foldMax is still the seed lo.
      -- Without the clamp this would return lo - cursorEpsilon and drift the persisted
      -- cursor backward a millisecond per starved pass.
      advanceWatermark 100 True 0 100 200 @?= 100
  ]

-- A frozen fold, whether from a spent budget or a transient failure, and a FULL page must
-- both land STRICTLY below the fold's max start_time. The next pass has a strictly exclusive
-- @after@, so this is what makes it re-fetch any event sharing that exact start_time rather
-- than skipping it. A short, drained page advances forward instead, never below the fold
-- max.
--
-- The fold max is generated strictly above lo so the tie-safe nudge is not swallowed by the
-- lo clamp; prop_advanceClampsLo pins that boundary case separately.
prop_advanceTieSafe :: Property
prop_advanceTieSafe = property $ do
  lo <- forAll (Gen.double (Range.linearFrac 0 1e9))
  foldMax <- forAll (Gen.double (Range.linearFrac (lo + 1) 2e9))
  hi <- forAll (Gen.double (Range.linearFrac 1 2e9))
  got <- forAll (Gen.int (Range.linear 0 499)) -- under the fetch limit (500)
  assert (advanceWatermark lo True got foldMax hi < foldMax) -- frozen holds below foldMax
  assert (advanceWatermark lo False (got + 500) foldMax hi < foldMax) -- full page holds below
  assert (advanceWatermark lo False got foldMax hi >= foldMax) -- short drained advances forward

-- The nudge is clamped at lo. A fold that stalls at or before its seed must hold exactly lo,
-- never lo - cursorEpsilon, so a run of starved passes cannot walk the persisted cursor
-- backward.
prop_advanceClampsLo :: Property
prop_advanceClampsLo = property $ do
  lo <- forAll (Gen.double (Range.linearFrac 1 2e9))
  foldMax <- forAll (Gen.double (Range.linearFrac 0 lo)) -- at or below the seed lo
  hi <- forAll (Gen.double (Range.linearFrac 1 2e9))
  assert (advanceWatermark lo True 0 foldMax hi >= lo) -- frozen never drifts below lo
  assert (advanceWatermark lo False 500 foldMax hi >= lo) -- full page never drifts below lo

-- | The background-work queue: submit coalesces identical jobs, distinct jobs each enqueue,
-- jobRunning is exact per job so each day's refresh polls only its own work, and a drained
-- job's key clears so it can be submitted again.
workerUnits :: [TestTree]
workerUnits =
  [ testProperty "submit coalesces an identical job; jobRunning is exact per job" prop_workerCoalesces
  , testCase "distinct jobs each enqueue (no coalescing across kinds)" $ do
      jobs <- Worker.newJobs
      a <- Worker.submit jobs Worker.RunBatch
      b <- Worker.submit jobs (Worker.BuildDay (fromGregorian 2026 7 9))
      (a, b) @?= (True, True)
  , testCase "jobRunning clears after the worker drains a job, and it can be resubmitted" workerDrain
  ]

genDay :: Gen Day
genDay =
  fromGregorian
    <$> Gen.integral (Range.linear 2020 2030)
    <*> Gen.int (Range.linear 1 12)
    <*> Gen.int (Range.linear 1 28)

genJob :: Gen Worker.Job
genJob = Gen.choice [pure Worker.RunBatch, Worker.BuildDay <$> genDay, pure Worker.RefreshBrief]

-- A job distinct from the given one, using a different day for the decoupling check. The
-- 1999 build can never collide with a generated 'BuildDay', whose years run 2020-2030.
otherJob :: Worker.Job -> Worker.Job
otherJob (Worker.BuildDay _) = Worker.RunBatch
otherJob _                   = Worker.BuildDay (fromGregorian 1999 1 1)

prop_workerCoalesces :: Property
prop_workerCoalesces = property $ do
  job <- forAll genJob
  (first, again, thisRunning, otherRunning) <- evalIO $ do
    jobs <- Worker.newJobs
    a <- Worker.submit jobs job
    b <- Worker.submit jobs job
    r1 <- Worker.jobRunning jobs job
    r2 <- Worker.jobRunning jobs (otherJob job)
    pure (a, b, r1, r2)
  first === True -- the first submit enqueues
  again === False -- an identical job in flight coalesces
  thisRunning === True -- the submitted job reads as running
  otherRunning === False -- a distinct job (another day) does not, so days stay decoupled

-- | Launch the worker over a trivial perform, submit a job, wait for it to run, then confirm
-- jobRunning clears, meaning the finally in runJobs removed the in-flight key, and that the
-- same job can be submitted afresh.
workerDrain :: IO ()
workerDrain = do
  jobs <- Worker.newJobs
  ran <- newEmptyMVar
  Async.withAsync (Worker.runJobs jobs (pipelineTracer renderingTracer) (\_ -> void (tryPutMVar ran ()))) $ \_ -> do
    _ <- Worker.submit jobs Worker.RunBatch
    takeMVar ran -- the worker executed the job
    waitUntil (not <$> Worker.jobRunning jobs Worker.RunBatch) -- its inflight key clears just after
    again <- Worker.submit jobs Worker.RunBatch
    again @?= True

-- Poll a condition to True, up to ~2s, failing the test otherwise.
waitUntil :: IO Bool -> IO ()
waitUntil check = go (200 :: Int)
  where
    go 0 = assertFailure "condition did not hold in time"
    go n = do
      ok <- check
      if ok then pure () else threadDelay 10000 >> go (n - 1)

props :: [TestTree]
props =
  [ testProperty "PetStat monoid is associative" prop_petStatAssoc
  , testProperty "PetStat monoid has identity" prop_petStatIdentity
  , testProperty "factsOf mirrors statOf (projection fidelity)" prop_factsMirrorStat
  , testProperty "identifyWith with no overrides == identify" prop_identifyWithRefines
  , testProperty "refresh resolves a day to a job (past->build, today/absent->batch, future/invalid->reject)" prop_resolveRefresh
  ]

genPetStat :: Gen PetStat
genPetStat =
  PetStat <$> n <*> n <*> n <*> n <*> n <*> n <*> n <*> n <*> n <*> n
  where
    n = Gen.int (Range.linear 0 20)

prop_petStatAssoc :: Property
prop_petStatAssoc = property $ do
  a <- forAll genPetStat
  b <- forAll genPetStat
  c <- forAll genPetStat
  (a <> b) <> c === a <> (b <> c)

prop_petStatIdentity :: Property
prop_petStatIdentity = property $ do
  a <- forAll genPetStat
  a <> emptyPetStat === a
  emptyPetStat <> a === a

genAppearance :: Gen Appearance
genAppearance = do
  w <- Gen.element [AnAnimal (Species "cat"), AnAnimal (Species "dog"), APerson]
  act <- Gen.enumBounded
  b <- genBehaviors
  pure (Appearance w act b Nothing)
  where
    genBehaviors = do
      a <- Gen.bool
      d <- Gen.bool
      s <- Gen.bool
      p <- Gen.bool
      g <- Gen.bool
      elim <- Gen.maybe (Elimination <$> Gen.enumBounded <*> Gen.enumBounded)
      cs <- Gen.list (Range.linear 0 2) genSignal
      pure
        noBehaviors
          { ate = a
          , drank = d
          , slept = s
          , played = p
          , groomed = g
          , eliminated = elim
          , concerns = cs
          }
    genSignal =
      Gen.element [PossibleInjury, Limping, Lethargy, OtherConcern "coughing"]

genScene :: Gen Scene
genScene = do
  aps <- Gen.list (Range.linear 0 3) genAppearance
  notable <- Gen.maybe (Gen.element ["a bark", "nothing unusual"])
  desc <- Gen.maybe (Gen.element ["A cat naps.", "An empty room."])
  wb <- Gen.enumBounded
  conf <- Gen.maybe (Gen.element (map mkConfidence [0, 0.25, 0.5, 0.75, 1]))
  pure (Scene aps notable desc wb conf)

genPerception :: Gen Perception
genPerception =
  Gen.choice
    [ Seen <$> genScene
    , Heard . SoundKind <$> Gen.element ["bark", "meow", "smoke_detector"]
    ]

genSceneEdit :: Gen SceneEdit
genSceneEdit =
  SceneEdit
    <$> Gen.maybe Gen.enumBounded
    <*> Gen.maybe Gen.enumBounded
    <*> Gen.maybe (Gen.element ["a note", "another"])
    <*> Gen.maybe (Gen.element ["here", "there"])
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe Gen.bool

genUTCTime :: Gen UTCTime
genUTCTime = UTCTime <$> genDay <*> (fromIntegral <$> Gen.int (Range.linear 0 86399))

-- | Invariants of the perception transforms: normalisation is idempotent, and the
-- owner corrections/edits touch only what they are meant to (never a person appearance
-- or a Heard sound, and an individual attribution never rewrites the stored blob).
perceptionUnits :: [TestTree]
perceptionUnits =
  [ testProperty "normalizeScene is idempotent" prop_normalizeIdempotent
  , testProperty "an individual attribution (pet/visiting) leaves the perception blob unchanged" prop_correctionIndividualKeepsBlob
  , testProperty "a species correction keeps persons and never touches a sound" prop_correctionKeepsPersonsAndSounds
  , testProperty "an edit leaves person appearances and sounds untouched" prop_editKeepsPersonsAndSounds
  ]

-- normalizeScene only repairs activity/behaviour consistency per appearance; running it
-- a second time finds nothing left to fix.
prop_normalizeIdempotent :: Property
prop_normalizeIdempotent = property $ do
  sc <- forAll genScene
  normalizeScene (normalizeScene sc) === normalizeScene sc

-- ToPet/ToVisiting are per-individual attributions stored as overrides at the DB layer;
-- they must never rewrite the model-facing perception blob.
prop_correctionIndividualKeepsBlob :: Property
prop_correctionIndividualKeepsBlob = property $ do
  p <- forAll genPerception
  applyCorrection ToVisiting p === p
  applyCorrection (ToPet (PetId "dexter")) p === p

-- A species correction retargets only ANIMAL appearances (each stays an animal), leaves
-- every person a person, and never touches a Heard sound.
prop_correctionKeepsPersonsAndSounds :: Property
prop_correctionKeepsPersonsAndSounds = property $ do
  sc <- forAll genScene
  k <- forAll (Gen.element ["bark", "meow"])
  applyCorrection (ToSpecies (Species "dog")) (Heard (SoundKind k)) === Heard (SoundKind k)
  case applyCorrection (ToSpecies (Species "dog")) (Seen sc) of
    Seen sc' -> map (isPerson . who) (appearances sc') === map (isPerson . who) (appearances sc)
    Heard _  -> failure

-- An owner edit applies to animal appearances and scene-level fields only: a person
-- appearance is passed through untouched, and a Heard sound is never edited.
prop_editKeepsPersonsAndSounds :: Property
prop_editKeepsPersonsAndSounds = property $ do
  sc <- forAll genScene
  e <- forAll genSceneEdit
  k <- forAll (Gen.element ["bark", "meow"])
  applyEdit e (Heard (SoundKind k)) === Heard (SoundKind k)
  case applyEdit e (Seen sc) of
    Seen sc' -> [a | a <- appearances sc', isPerson (who a)] === [a | a <- appearances sc, isPerson (who a)]
    Heard _  -> failure

-- | 'nextBatchTime' returns the earliest configured hour strictly after now, or Nothing
-- exactly when no hours are configured; it never lands at or before now.
schedulerUnits :: [TestTree]
schedulerUnits =
  [ testProperty "nextBatchTime is strictly after now (and Nothing iff no hours)" prop_nextBatchAfterNow
  ]

prop_nextBatchAfterNow :: Property
prop_nextBatchAfterNow = property $ do
  hours <- forAll (mapMaybe mkHour <$> Gen.list (Range.linear 0 5) (Gen.int (Range.linear 0 23)))
  now <- forAll genUTCTime
  case nextBatchTime hours utcTZ now of
    Nothing -> assert (null hours)
    Just t  -> assert (not (null hours) && t > now)

-- The DB facts projection must never disagree with the domain stats it is built
-- from, so aggregates over the table equal aggregates over 'statOf'.
prop_factsMirrorStat :: Property
prop_factsMirrorStat = property $ do
  ap <- forAll genAppearance
  case factsOf (Seen emptyScene {appearances = [ap]}) of
    [f] -> do
      let st = statOf ap
      sfActivity f === Just (activityText (activity ap))
      sfRest f === (psRest st > 0)
      sfActive f === (psActive st > 0)
      sfAte f === (psAte st > 0)
      sfDrank f === (psDrank st > 0)
      sfSlept f === (psSlept st > 0)
      sfPlayed f === (psPlayed st > 0)
      sfGroomed f === (psGroomed st > 0)
      sfEliminated f === (psEliminated st > 0)
      sfAccident f === accidentSuspected (behaviors ap)
      sfConcern f === (psConcerns st > 0)
    _ -> failure

-- The override-aware resolver must reduce to plain 'identify' when there are no
-- overrides, so every existing attribution is preserved and only owner
-- corrections change the outcome.
prop_identifyWithRefines :: Property
prop_identifyWithRefines = property $ do
  ap <- forAll genAppearance
  let roster = [Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing]
  identifyWith mempty roster (0, 0) ap === identify roster ap

-- The /api/refresh day argument maps to a background job: absent or today runs the
-- full batch, a valid past day builds exactly that day, and a future or garbage day
-- is rejected (so a 'started' response is never returned for work that will not run).
prop_resolveRefresh :: Property
prop_resolveRefresh = property $ do
  let now = UTCTime (fromGregorian 2026 7 14) 43200
      today = fromGregorian 2026 7 14
      run = resolveRefresh utcTZ now
      iso d = pack (show d)
  run Nothing === Right Worker.RunBatch
  run (Just "today") === Right Worker.RunBatch
  pastN <- forAll (Gen.integral (Range.linear 1 400))
  let pd = addDays (negate pastN) today
  run (Just (iso pd)) === Right (Worker.BuildDay pd)
  futN <- forAll (Gen.integral (Range.linear 1 400))
  run (Just (iso (addDays futN today))) === Left "day is in the future"
  isLeft (run (Just "not-a-day")) === True

enumTrip ::
  (Show a, Eq a, ToJSON a, FromJSON a) => Gen a -> Property
enumTrip g = property $ do
  x <- forAll g
  tripping x encode decode

prop_confidenceClamped :: Property
prop_confidenceClamped = property $ do
  d <- forAll (Gen.double (Range.linearFrac (-5) 5))
  let c = confidenceValue (mkConfidence d)
  assert (c >= 0 && c <= 1)

domainUnits :: [TestTree]
domainUnits =
  [ testCase "window from <= to, even when inverted" $ do
      let now = UTCTime (fromGregorian 2026 7 8) 43200
          w = parseWindow utcTZ now (Just "7d") (Just "today")
          wInv = parseWindow utcTZ now (Just "today") (Just "7d")
      assertLE (winFrom w) (winTo w)
      assertLE (winFrom wInv) (winTo wInv)
  , testCase "a from..to window spans every day in the range, inclusive" $ do
      let now = UTCTime (fromGregorian 2026 7 13) 43200
          -- The range endpoint passes two distinct day args; the window must cover
          -- both ends and everything between, and nothing on either side.
          w = parseWindow utcTZ now (Just "2026-07-06") (Just "2026-07-08")
          noon dd = UTCTime (fromGregorian 2026 7 dd) 43200
          within t = winFrom w <= t && t < winTo w
      map (within . noon) [6, 7, 8] @?= [True, True, True]
      map (within . noon) [5, 9] @?= [False, False]
  , testCase "an Nd arg is bounded to [1,3650]; out-of-range reads as unrecognized" $ do
      let now = UTCTime (fromGregorian 2026 7 8) 43200
      -- In range: recognized, and resolves to a window that far back.
      map recognizedArg ["1d", "3650d"] @?= [True, True]
      winFrom (parseWindow utcTZ now (Just "2d") Nothing)
        @?= startOfLocalDay utcTZ (fromGregorian 2026 7 6)
      -- Out of range (0, over a decade, or an absurd overflow-scale value from an
      -- Ask-tool arg): unrecognized, and parseWindow falls back to its default (today's
      -- midnight for the since bound) rather than a nonsensical window.
      map recognizedArg ["0d", "3651d", "99999999999999999999d"] @?= [False, False, False]
      winFrom (parseWindow utcTZ now (Just "99999999999999999999d") Nothing)
        @?= startOfLocalDay utcTZ (fromGregorian 2026 7 8)
  , testCase "identify resolves against the roster" $ do
      identify roster apCat @?= KnownPet dexter
      identify roster apBird @?= UnknownAnimal (Species "bird")
      identify roster apPerson @?= Human
  , testCase "eating implies ate, not drank" $ do
      ate (normalizeBehaviors Eating noBehaviors) @?= True
      drank (normalizeBehaviors Eating noBehaviors) @?= False
  , testCase "accident only for inappropriate elimination" $ do
      accidentSuspected (elim Urine Inappropriate) @?= True
      accidentSuspected (elim Feces LitterBox) @?= False
  , testCase "scene schema is a JSON object" $
      case sceneSchema of
        Object _ -> pure ()
        _        -> assertFailure "sceneSchema is not a JSON object"
  , testCase "presence attributes behaviours to the right pet" $ do
      let sc = emptyScene {appearances = [eatingCat]}
          obs = Observation (ObsId 1) t0 (Camera "office") PeriodicSample (Seen sc) False
          m = presence mempty roster [obs]
      fmap psAte (Map.lookup (KPet (PetId "dexter")) m) @?= Just 1
      fmap psSightings (Map.lookup (KPet (PetId "dexter")) m) @?= Just 1
  , testCase "an override attributes a same-species sighting to a specific pet" $ do
      let miso = Pet (PetId "miso") "Miso" (Species "cat") "grey cat" Nothing Nothing Nothing
          mochi = Pet (PetId "mochi") "Mochi" (Species "cat") "black cat" Nothing Nothing Nothing
          twoCat = [miso, mochi]
          catAp = Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing
          ov = Map.fromList [((7, 0), IdPet (PetId "mochi"))]
      -- Two cats are ambiguous without an override, so neither is named.
      identify twoCat catAp @?= UnknownAnimal (Species "cat")
      -- The override resolves this exact sighting to the specific cat.
      identifyWith ov twoCat (7, 0) catAp @?= KnownPet mochi
      -- A different observation is unaffected.
      identifyWith ov twoCat (8, 0) catAp @?= UnknownAnimal (Species "cat")
  , testCase "an archived pet is excluded from auto-identification, but its corrections still resolve" $ do
      let active = Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing
          gone = Pet (PetId "misty") "Misty" (Species "cat") "grey cat" Nothing (Just t0) Nothing
          roster' = [active, gone]
          catAp = Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing
      -- Two cats on paper, but one is archived, so the sole active cat is named.
      identify roster' catAp @?= KnownPet active
      -- An old correction to the archived pet still resolves to its name.
      identifyWith (Map.fromList [((1, 0), IdPet (PetId "misty"))]) roster' (1, 0) catAp @?= KnownPet gone
  , testCase "presence credits an override to the specific pet only" $ do
      let miso = Pet (PetId "miso") "Miso" (Species "cat") "grey cat" Nothing Nothing Nothing
          mochi = Pet (PetId "mochi") "Mochi" (Species "cat") "black cat" Nothing Nothing Nothing
          twoCat = [miso, mochi]
          sc = emptyScene {appearances = [eatingCat]}
          obs = Observation (ObsId 3) t0 (Camera "office") PeriodicSample (Seen sc) False
          ov = Map.fromList [((3, 0), IdPet (PetId "mochi"))]
          m = presence ov twoCat [obs]
      fmap psAte (Map.lookup (KPet (PetId "mochi")) m) @?= Just 1
      Map.lookup (KPet (PetId "miso")) m @?= Nothing
      Map.lookup (KSpecies (Species "cat")) m @?= Nothing
  , testCase "a visitor override is excluded from the pet's stats" $ do
      let sc = emptyScene {appearances = [eatingCat]}
          obs = Observation (ObsId 9) t0 (Camera "office") PeriodicSample (Seen sc) False
          ov = Map.fromList [((9, 0), IdVisiting)]
          m = presence ov roster [obs]
      identifyWith ov roster (9, 0) eatingCat @?= Visiting (Species "cat")
      Map.lookup (KPet (PetId "dexter")) m @?= Nothing
      fmap psSightings (Map.lookup (KVisitor (Species "cat")) m) @?= Just 1
  , testCase "homePresence detects a person and ignores animals" $ do
      let personObs = Observation (ObsId 1) t0 (Camera "office") PeriodicSample (Seen emptyScene {appearances = [apPerson]}) False
          catObs = Observation (ObsId 2) t0 (Camera "office") PeriodicSample (Seen emptyScene {appearances = [apCat]}) False
      prSomeoneHome (homePresence [catObs]) @?= False
      prSomeoneHome (homePresence [personObs, catObs]) @?= True
      prPersonSightings (homePresence [personObs, catObs]) @?= 1
  , testCase "applyEdit overwrites set fields and keeps the rest" $ do
      let sc = emptyScene {appearances = [apCat], description = Just "old"}
          e =
            noEdit
              { seActivity = Just Eating
              , seAte = Just True
              , seDescription = Just "new note"
              , seWellbeing = Just Concerning
              }
      case applyEdit e (Seen sc) of
        Seen sc' -> do
          description sc' @?= Just "new note"
          wellbeing sc' @?= Concerning
          fmap activity (listToMaybe (appearances sc')) @?= Just Eating
          fmap (ate . behaviors) (listToMaybe (appearances sc')) @?= Just True
          fmap (slept . behaviors) (listToMaybe (appearances sc')) @?= Just False
        Heard _ -> assertFailure "expected a scene"
  , testCase "applyEdit clears an optional field on blank" $ do
      let sc = emptyScene {appearances = [apCat], description = Just "old"}
      case applyEdit noEdit {seDescription = Just "   "} (Seen sc) of
        Seen sc' -> description sc' @?= Nothing
        Heard _  -> assertFailure "expected a scene"
  , testCase "boundedLines keeps recent lines and notes the drop" $ do
      boundedLines 3 ["a", "b", "c"] @?= "a\nb\nc"
      boundedLines 2 ["a", "b", "c", "d"] @?= "(2 earlier lines omitted)\nc\nd"
  , testCase "soundPhrase renders labels; safety sounds are classified" $ do
      soundPhrase (SoundKind "smoke_detector") @?= "a smoke alarm"
      soundPhrase (SoundKind "doorbell") @?= "the doorbell"
      soundPhrase (SoundKind "whimper_dog") @?= "a whimper"
      isSafetySound (SoundKind "smoke_detector") @?= True
      isSafetySound (SoundKind "bark") @?= False
  , testCase "applyEdit is idempotent" $ do
      let sc = emptyScene {appearances = [apCat]}
          e = noEdit {seActivity = Just Playing, seAte = Just True}
          once = applyEdit e (Seen sc)
      applyEdit e once @?= once
  ]
  where
    dexter = Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing
    yuki = Pet (PetId "yuki") "Yuki" (Species "dog") "three-legged dog" (Just "3 legs") Nothing Nothing
    roster = [dexter, yuki]
    noEdit = SceneEdit Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    apCat = Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing
    apBird = Appearance (AnAnimal (Species "bird")) Alert noBehaviors Nothing
    apPerson = Appearance APerson Standing noBehaviors Nothing
    eatingCat =
      Appearance
        (AnAnimal (Species "cat"))
        Eating
        (normalizeBehaviors Eating noBehaviors)
        Nothing
    elim t p = noBehaviors {eliminated = Just (Elimination t p)}
    t0 = UTCTime (fromGregorian 2026 7 8) 0
    assertLE a b =
      if a <= b then pure () else assertFailure "expected from <= to"

viewUnits :: [TestTree]
viewUnits =
  [ testCase "viewOf resolves subject, room, and chips" $ do
      let v = mkView (emptyScene {appearances = [eatingCat], confidence = Just (mkConfidence 0.9)})
      ovSubjectLabel v @?= "Dexter"
      ovRoom v @?= "Office"
      elem "ate" (map chLabel (ovChips v)) @?= True
      ovUncertain v @?= False
  , testCase "low confidence marks a card uncertain" $ do
      let v = mkView (emptyScene {appearances = [eatingCat], confidence = Just (mkConfidence 0.4)})
      ovUncertain v @?= True
  , testCase "chipsOf flags visitor and notable" $ do
      let sc = emptyScene {appearances = [eatingCat, personAp], notable = Just "doorbell"}
          ls = map chLabel (chipsOf sc)
      elem "visitor" ls @?= True
      elem "notable" ls @?= True
  , testCase "hourHistogram buckets by local hour" $ do
      let obsH = Observation (ObsId 2) tHour (Camera "office") PeriodicSample (Seen sc1) False
          hh = hourHistogram utcTZ mempty (KPet (PetId "dexter")) roster [obsH]
      length hh @?= 24
      (hh !! 14) @?= 1
  , testCase "roomDistribution counts per room" $
      roomDistribution [] mempty roster (PetId "dexter") [obs1, obs1] @?= [("Office", 2)]
  , testCase "petStatOver is monotone over nested windows (day <= week <= month)" $ do
      -- One sighting today, one 2 days ago, one 9 days ago: the 1/7/30-day windows
      -- must count 1/2/3, so day <= week <= month holds by construction.
      let mkO oid daysBack =
            Observation
              (ObsId oid)
              (addUTCTime (negate (fromIntegral (daysBack :: Int) * 86400)) t0)
              (Camera "office")
              PeriodicSample
              (Seen sc1)
              False
          obss = [mkO 1 0, mkO 2 2, mkO 3 9]
          seenOver n = psSightings (petStatOver utcTZ mempty roster n t0 dexter obss)
      seenOver 1 @?= 1
      seenOver 7 @?= 2
      seenOver 30 @?= 3
  , testCase "householdVisionNote surfaces visitor + cat-flap context (empty when off)" $ do
      householdVisionNote (Household False False Nothing Nothing) @?= ""
      let note = householdVisionNote (Household True True Nothing Nothing)
      ("visitor" `isInfixOf` note) @?= True
      ("cat flap" `isInfixOf` note) @?= True
  , testCase "fallbackDescription prefers location, else the room" $ do
      fallbackDescription "Miso" (Just "resting") (Just "on the sofa") "Living Room"
        @?= "Miso resting on the sofa."
      fallbackDescription "Miso" (Just "sleeping") Nothing "Living Room"
        @?= "Miso sleeping in the Living Room."
      fallbackDescription "Unclear" Nothing Nothing "Office" @?= "Unclear in the Office."
  , testCase "viewOf synthesises a description when the model omits one" $
      ovDescription (mkView (emptyScene {appearances = [eatingCat]}))
        @?= Just "Dexter eating in the Office."
  , testCase "viewOf renders a safety sound as a concerning moment" $ do
      let ev = FromEvent (FrigateMeta (EventId "e1") "smoke_detector" 0.9 False False)
          v =
            viewOf
              roster
              mempty
              (roomOf [])
              stubMedia
              (Observation (ObsId 3) t0 (Camera "office") ev (Heard (SoundKind "smoke_detector")) False)
      ovSubjectLabel v @?= "A sound"
      ovWellbeing v @?= "concerning"
      ovDescription v @?= Just "Heard a smoke alarm"
      elem "safety" (map chLabel (ovChips v)) @?= True
  , testCase "wellbeingLine hides a thin week, shows the shape of a full one" $ do
      -- Under 5 sightings this week is too thin to say anything.
      wellbeingLine "Dexter" 4 30 (Just "Office") @?= Nothing
      -- At/over the threshold the line carries the name, count and rest pct.
      case wellbeingLine "Dexter" 5 30 (Just "Office") of
        Nothing -> assertFailure "expected a wellbeing line at 5 sightings"
        Just l -> do
          ("Dexter" `isInfixOf` l) @?= True
          ("5 times this week" `isInfixOf` l) @?= True
          ("30%" `isInfixOf` l) @?= True
          -- The favourite room is named when present.
          ("most often in the Office" `isInfixOf` l) @?= True
  , testCase "wellbeingLine never claims a moment is flagged" $
      -- It used to append "One moment is flagged for a look" for any watch verdict, which
      -- was false when the verdict came from a missed meal: there is no such moment, and it
      -- pointed the owner at a review queue that could be empty. The reason now lives on the
      -- note and glance chips, which name it.
      case wellbeingLine "Miso" 8 20 Nothing of
        Nothing -> assertFailure "expected a wellbeing line at 8 sightings"
        Just l -> do
          ("flagged for a look" `isInfixOf` l) @?= False
          -- No top spot, so no "most often in" clause.
          ("most often in" `isInfixOf` l) @?= False
  , testCase "mkRecap pins Seen/Days seen to the live week, passes the rest through" $ do
      let s =
            Db.PetSummary
              Settled
              (Just "A calm week.")
              [("Seen", "99 times"), ("Days seen", "2 of 7"), ("Ate", "4 sightings")]
      case mkRecap 12 5 s of
        Nothing -> assertFailure "expected a recap when the summary has a line"
        Just r -> do
          rcRange r @?= "this week"
          rcLine r @?= "A calm week."
          rcStats r
            @?= [ StatPair "Seen" "12 this week"
                , StatPair "Days seen" "5 of 7"
                , StatPair "Ate" "4 sightings"
                ]
      -- A summary with no recap line yields nothing at all.
      mkRecap 12 5 (Db.PetSummary Settled Nothing [("Seen", "1 time")]) @?= Nothing
  , testCase "mediaFor resolves each origin/perception branch" $ do
      -- A sound event with a saved clip serves the clip as audio; without one, nothing.
      omClip (mediaFor 30 t0 (heardObs (Just clipEvent))) @?= Just "/api/events/e1/clip.mp4"
      omKind (mediaFor 30 t0 (heardObs (Just clipEvent))) @?= "audio"
      mediaFor 30 t0 (heardObs Nothing) @?= ObsMedia "audio" Nothing Nothing
      -- A seen moment older than the retention window is expired (tidied away).
      omKind (mediaFor 30 nowMonthLater (seenSample t0)) @?= "expired"
      omKind (mediaFor 30 t0 (seenSample t0)) @?= "photo"
      -- A periodic sample serves its proof frame as a photo.
      mediaFor 30 t0 (seenSample t0)
        @?= ObsMedia "photo" (Just ("/proof/office/" <> tsText t0 <> ".jpg")) Nothing
      -- A clip-bearing event serves snapshot + clip; a snapshot-only event a photo.
      mediaFor 30 t0 (seenEvent (Just clipEvent))
        @?= ObsMedia "clip" (Just "/api/events/e1/snapshot.jpg") (Just "/api/events/e1/clip.mp4")
      mediaFor 30 t0 (seenEvent (Just snapOnlyEvent))
        @?= ObsMedia "photo" (Just "/api/events/e2/snapshot.jpg") Nothing
  , testCase "mkKeepsake lifts a saved moment, resolving its media" $ do
      let k = Db.Keepsake 7 1 (Just "dexter") (Just "nap") t0
          kv = mkKeepsake 30 [] t0 k (seenSample t0)
      kpId kv @?= 7
      kpObsId kv @?= 1
      kpCaption kv @?= Just "nap"
      kpRoom kv @?= "Office"
      kpMedia kv @?= "photo"
      kpImg kv @?= Just ("/proof/office/" <> tsText t0 <> ".jpg")
  , testCase "resolveCorrection follows visiting > person > pet > species precedence" $ do
      -- Visiting wins over everything set at once.
      resolveCorrection roster (target (Just "dexter") (Just "cat") (Just True) (Just True))
        @?= Just ToVisiting
      -- Person is next when visiting is unset.
      resolveCorrection roster (target (Just "dexter") (Just "cat") (Just True) Nothing)
        @?= Just ToPerson
      -- A roster pet id is next; it must actually exist in the roster.
      resolveCorrection roster (target (Just "dexter") (Just "cat") Nothing Nothing)
        @?= Just (ToPet (PetId "dexter"))
      -- Species last, when nothing higher is set.
      resolveCorrection roster (target Nothing (Just "cat") Nothing Nothing)
        @?= Just (ToSpecies (Species "cat"))
      -- All unset resolves to nothing.
      resolveCorrection roster (target Nothing Nothing Nothing Nothing) @?= Nothing
      -- A pet id NOT in the roster falls through to the species rather than naming it.
      resolveCorrection roster (target (Just "ghost") (Just "cat") Nothing Nothing)
        @?= Just (ToSpecies (Species "cat"))
  ]
  where
    dexter = Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing
    roster = [dexter]
    eatingCat =
      Appearance (AnAnimal (Species "cat")) Eating (normalizeBehaviors Eating noBehaviors) Nothing
    personAp = Appearance APerson Standing noBehaviors Nothing
    sc1 = emptyScene {appearances = [eatingCat]}
    obs1 = Observation (ObsId 1) t0 (Camera "office") PeriodicSample (Seen sc1) False
    tHour = UTCTime (fromGregorian 2026 7 8) 50400
    stubMedia = const (ObsMedia "photo" Nothing Nothing)
    mkView sc =
      viewOf
        roster
        mempty
        (roomOf [])
        stubMedia
        (Observation (ObsId 1) t0 (Camera "office") PeriodicSample (Seen sc) False)
    t0 = UTCTime (fromGregorian 2026 7 8) 0
    -- A time well past the 30-day retention window, for the "expired" branch.
    nowMonthLater = addUTCTime (60 * 86400) t0
    clipEvent = FrigateMeta (EventId "e1") "cat" 0.9 True True
    snapOnlyEvent = FrigateMeta (EventId "e2") "cat" 0.9 False True
    heardObs mev =
      Observation (ObsId 1) t0 (Camera "office") (maybe PeriodicSample FromEvent mev) (Heard (SoundKind "bark")) False
    seenSample tt = Observation (ObsId 1) tt (Camera "office") PeriodicSample (Seen sc1) False
    seenEvent mev = Observation (ObsId 1) t0 (Camera "office") (maybe PeriodicSample FromEvent mev) (Seen sc1) False
    -- The POSIX-seconds stamp mediaFor embeds in a proof URL, as text.
    tsText tt = pack (show (round (utcTimeToPOSIXSeconds tt) :: Integer))
    target = CorrectionTarget

-- | The JSON contract the frontend mirrors: pin the exact top-level keys the
-- backend emits for the complex response types, so a field rename/add/remove
-- (which would silently break the hand-written client) fails a test instead.
-- These come straight from frontend/src/lib/api.ts.
contractUnits :: [TestTree]
contractUnits =
  [ testCase "ObsView JSON keys match the contract Moment model" $
      jsonKeys (encode ov)
        @?= sort
          [ "id", "at", "camera", "room", "subjectLabel", "subjects", "activity"
          , "location", "chips", "wellbeing", "confidence", "uncertain"
          , "description", "media", "transcript", "needsReview"
          , "reviewed", "kept", "clipExpiresAt"
          ]
  , testCase "ObsView media is a nested { kind, stillUrl, clipUrl } object" $
      jsonKeys (nestedKey "media" (encode ov))
        @?= sort ["kind", "stillUrl", "clipUrl"]
  , testCase "PetInsights JSON keys match the client contract" $
      jsonKeys (encode ins)
        @?= sort
          [ "id", "name", "species", "description", "caveat", "seen", "glance"
          , "note", "tiles", "spark", "habits", "rhythm", "rhythmMarks", "balance"
          , "spots", "wellbeing", "lastSeen", "recap", "keepsake", "monthSeen"
          , "monthStats", "anomaly"
          ]
  -- DayResponse and its nested Presence: keys from frontend/src/lib/api.ts
  , testCase "DayResponse JSON keys match the client contract" $
      jsonKeys (encode dayResp)
        @?= sort ["date", "moments", "narrative", "presence", "pets"]
  , testCase "Presence JSON keys match the client contract" $
      jsonKeys (nestedKey "presence" (encode dayResp))
        @?= sort ["someoneHome", "personSightings", "lastPersonAt"]
  -- Profile and its nested sub-shapes: keys from frontend/src/lib/api.ts
  , testCase "Profile JSON keys match the client contract" $
      jsonKeys (encode emptyProfile)
        @?= sort
          [ "pets", "report", "household", "cameras"
          , "frigateUrl", "modelUrl", "visionModel", "timeZone", "gcWindowDays"
          , "captureSecs", "configuredAt"
          ]
  , testCase "Pet JSON keys match the client contract" $
      jsonKeys (encode (Pet (PetId "p") "P" (Species "cat") "desc" Nothing Nothing Nothing))
        @?= sort
          ["petId", "petName", "petSpecies", "petDescription", "petNotes", "petArchivedAt", "petPhoto"]
  , testCase "ReportPrefs JSON keys match the client contract" $
      jsonKeys (encode (report emptyProfile))
        @?= sort ["topics", "freeform"]
  , testCase "Household JSON keys match the client contract" $
      jsonKeys (encode (household emptyProfile))
        @?= sort ["catFlap", "neighbourCat", "feedTimes", "notes"]
  , testCase "CameraRoom JSON keys match the client contract" $
      jsonKeys (encode (CameraRoom "cam" "room" True))
        @?= sort ["camId", "room", "enabled"]
  -- DiscoveredCamera / CameraInfo: keys from frontend/src/lib/api.ts
  , testCase "CameraInfo JSON keys match the client contract" $
      jsonKeys (encode (CameraInfo "cam" True))
        @?= sort ["name", "online"]
  -- AskAnswer / AskResp: keys from frontend/src/lib/api.ts
  , testCase "AskResp JSON keys match the client contract" $
      jsonKeys (encode (AskResp "answer" []))
        @?= sort ["answer", "refs"]
  -- Keepsake (Db.Keepsake, prefixed 1: k -> id/obsId/...) -- from frontend/src/lib/api.ts
  , testCase "Keepsake JSON keys match the client contract" $
      jsonKeys (encode (Db.Keepsake 1 2 Nothing Nothing t0'))
        @?= sort ["id", "obsId", "petId", "caption", "at"]
  ]
  where
    dexter = Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing
    t0' = UTCTime (fromGregorian 2026 7 8) 0
    catAp = Appearance (AnAnimal (Species "cat")) Eating (normalizeBehaviors Eating noBehaviors) Nothing
    obs = Observation (ObsId 1) t0' (Camera "office") PeriodicSample (Seen emptyScene {appearances = [catAp]}) False
    ov = viewOf [dexter] mempty (roomOf []) (const (ObsMedia "photo" Nothing Nothing)) obs
    ins = insightsFor utcTZ mempty [] [dexter] t0' dexter [obs]
    dayResp = DayResponse "2026-07-08" [] Nothing (PresenceV False 0 Nothing) []

-- | Pin the closed WIRE vocabularies the frontend (frontend/src/lib/api.ts) and
-- the DB cache mirror: the exact string set each enum serialises to. A future
-- rename of any constructor's encoding breaks a test HERE, at the backend, before
-- it can silently diverge from the hand-written client or a stored column value.
vocabularyUnits :: [TestTree]
vocabularyUnits =
  [ testCase "WellbeingKind encodes to exactly {good, watch} and round-trips (tolerant default)" $ do
      -- The verdict vocabulary the pet_summaries column and PetInsights JSON speak.
      map wellbeingKindText [minBound .. maxBound] @?= ["good", "watch"]
      map (wellbeingKindFromText . wellbeingKindText) [minBound .. maxBound]
        @?= [minBound .. maxBound]
      wellbeingKindFromText "watch" @?= Flagged
      wellbeingKindFromText "good" @?= Settled
      -- A stale/unknown cached value must read as the neutral verdict, not crash.
      wellbeingKindFromText "gibberish" @?= Settled
  , testCase "Severity labels are exactly {DEBUG, INFO, WARN, ERROR} and round-trip" $ do
      -- Two vocabularies that must agree: the label printed on every log line, and the name
      -- PET_REPORT_LOG_LEVEL accepts. Both derive from sevText, so this pins the spelling and
      -- the derivation together; a fifth level cannot desync them.
      map sevText [minBound .. maxBound] @?= ["DEBUG", "INFO", "WARN", "ERROR"]
      map (severityFromText . sevText) [minBound .. maxBound]
        @?= map Just [minBound .. maxBound]
      -- Case and surrounding space are ignored, so a level copied out of a log line works.
      severityFromText " Warn " @?= Just Warning
      -- The label is "warn", not "warning": an unrecognised name is Nothing, and
      -- logLevelFromEnv falls back to Info rather than failing to start.
      severityFromText "warning" @?= Nothing
      severityFromText "" @?= Nothing
  , testCase "PET_REPORT_LOG_LEVEL: unset defaults to info, a bad name defaults AND reports" $ do
      -- The pure core of logLevelFromEnv, so the fallback is pinned without mutating the
      -- process environment under a parallel test runner.
      resolveLogLevel Nothing @?= (Info, Nothing)
      resolveLogLevel (Just "   ") @?= (Info, Nothing)
      resolveLogLevel (Just "debug") @?= (Debug, Nothing)
      resolveLogLevel (Just " Warn ") @?= (Warning, Nothing)
      -- The label is WARN, so "warning" is a natural typo. It still falls back to Info, but
      -- the value comes back for the caller to report rather than being silently dropped.
      resolveLogLevel (Just "warning") @?= (Info, Just "warning")
  , testCase "RunOutcome encodes to exactly {ok, error, skipped}" $
      -- The last-batch status blob batchH reads back for the UI.
      map runOutcomeText [RanOk, RanError, RanSkipped] @?= ["ok", "error", "skipped"]
  , testCase "ObsMedia omKind is exactly {photo, clip, audio, expired} across every producing branch" $ do
      -- mediaFor is the sole producer of omKind; drive every origin/perception/age
      -- branch and pin the full set, so a fifth kind (or a rename) fails here.
      let base = UTCTime (fromGregorian 2026 7 8) 0
          monthLater = addUTCTime (60 * 86400) base
          catAp' = Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing
          seenSc = emptyScene {appearances = [catAp']}
          clipEv = FrigateMeta (EventId "e1") "cat" 0.9 True True
          snapEv = FrigateMeta (EventId "e2") "cat" 0.9 False True
          -- A clip-only event: Frigate saved a clip but no still (has_snapshot=false).
          clipOnlyEv = FrigateMeta (EventId "e3") "cat" 0.9 True False
          seenOb org tt = Observation (ObsId 1) tt (Camera "office") org (Seen seenSc) False
          heardOb org = Observation (ObsId 1) base (Camera "office") org (Heard (SoundKind "bark")) False
          kinds =
            [ omKind (mediaFor 30 base (seenOb PeriodicSample base)) -- photo (sample)
            , omKind (mediaFor 30 base (seenOb (FromEvent clipEv) base)) -- clip (event + clip)
            , omKind (mediaFor 30 base (seenOb (FromEvent snapEv) base)) -- photo (snapshot-only event)
            , omKind (mediaFor 30 base (heardOb PeriodicSample)) -- audio (sound)
            , omKind (mediaFor 30 monthLater (seenOb PeriodicSample base)) -- expired (aged out)
            ]
      sort (nubText kinds) @?= sort ["photo", "clip", "audio", "expired"]
      -- A clip-only event still plays its clip but advertises no snapshot poster (which
      -- would 404); a normal clip event keeps its snapshot poster.
      omImg (mediaFor 30 base (seenOb (FromEvent clipOnlyEv) base)) @?= Nothing
      omClip (mediaFor 30 base (seenOb (FromEvent clipOnlyEv) base))
        @?= Just "/api/events/e3/clip.mp4"
      omImg (mediaFor 30 base (seenOb (FromEvent clipEv) base))
        @?= Just "/api/events/e1/snapshot.jpg"
  , testCase "momentExpiry: an event counts down to the EARLIER of GC and the clip; a sample doesn't" $ do
      let base = UTCTime (fromGregorian 2026 7 8) 0
          ret = Map.fromList [("office", 10)]
          catAp' = Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing
          seenSc = emptyScene {appearances = [catAp']}
          clipEv = FrigateMeta (EventId "e1") "cat" 0.9 True True
          ev = Observation (ObsId 1) base (Camera "office") (FromEvent clipEv) (Seen seenSc) False
          sample = Observation (ObsId 2) base (Camera "office") PeriodicSample (Seen seenSc) False
      -- GC window wider than the clip (30 vs 10): the clip prunes first, so 10 days.
      momentExpiry ret 30 ev @?= Just (addUTCTime (10 * 86400) base)
      -- A short GC window (5) beats the clip retention (10): GC removes it first, 5 days.
      momentExpiry ret 5 ev @?= Just (addUTCTime (5 * 86400) base)
      -- Unknown clip retention: the GC window still gives a deadline.
      momentExpiry mempty 7 ev @?= Just (addUTCTime (7 * 86400) base)
      -- A periodic sample is pet-report's own media, not borrowed: no countdown.
      momentExpiry ret 5 sample @?= Nothing
  , testCase "ObsView wellbeing is exactly {normal, concerning, unclear, none} across every Wellbeing case" $ do
      -- The card wellbeing string is derived from the Wellbeing sum (Seen: its three
      -- cases via wellbeingText) plus the audio "none" path; pin the full set.
      let base = UTCTime (fromGregorian 2026 7 8) 0
          catAp' = Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing
          stubMedia = const (ObsMedia "photo" Nothing Nothing)
          seenWith w =
            viewOf
              [dexter]
              mempty
              (roomOf [])
              stubMedia
              (Observation (ObsId 1) base (Camera "office") PeriodicSample (Seen emptyScene {appearances = [catAp'], wellbeing = w}) False)
          heardView =
            viewOf
              [dexter]
              mempty
              (roomOf [])
              stubMedia
              (Observation (ObsId 1) base (Camera "office") PeriodicSample (Heard (SoundKind "bark")) False)
          wells = map (ovWellbeing . seenWith) [minBound .. maxBound] ++ [ovWellbeing heardView]
      sort (nubText wells) @?= sort ["normal", "concerning", "unclear", "none"]
  ]
  where
    dexter = Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing
    nubText = Map.keys . Map.fromList . map (\t -> (t, ()))

-- | Drill into a nested JSON object by key and return its top-level keys,
-- for pinning a nested shape without over-engineering a lens.
nestedKey :: Text -> LBS.ByteString -> LBS.ByteString
nestedKey k bs = case decode bs of
  Just (Object o) -> case KeyMap.lookup (Key.fromText k) o of
    Just v  -> encode v
    Nothing -> encode (Object KeyMap.empty)
  _ -> encode (Object KeyMap.empty)

-- | The sorted top-level object keys of an encoded JSON value ([] if not an
-- object), for contract assertions.
jsonKeys :: LBS.ByteString -> [Text]
jsonKeys bs = case decode bs of
  Just (Object o) -> sort (map Key.toText (KeyMap.keys o))
  _               -> []

-- | End-to-end against a real temporary SQLite database: the cheap SQL aggregate
-- ('subjectStatsBetween', which LEFT JOINs the override table) must agree with
-- the blob-path 'presence' over the same window, so the two never disagree about
-- who a sighting belongs to. This is the load-bearing invariant of the identity
-- redesign; the SQL path is 'identifyWith' expressed relationally.
dbUnits :: [TestTree]
dbUnits =
  [ testCase "SQL subjectStatsBetween agrees with blob presence, honouring overrides" $
      withSystemTempDirectory "petreport-spec" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let miso = Pet (PetId "miso") "Miso" (Species "cat") "grey cat" Nothing Nothing Nothing
              mochi = Pet (PetId "mochi") "Mochi" (Species "cat") "black cat" Nothing Nothing Nothing
              twoCat = [miso, mochi]
              base = UTCTime (fromGregorian 2026 7 8) 0
              tk k = addUTCTime (fromInteger (k * 100)) base
              catAp eat =
                Appearance
                  (AnAnimal (Species "cat"))
                  (if eat then Eating else Sleeping)
                  (normalizeBehaviors (if eat then Eating else Sleeping) noBehaviors)
                  Nothing
              obsAt k eat =
                NewObservation
                  (tk k)
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [catAp eat]})
          mapM_ (Db.insertObservation h) [obsAt 1 True, obsAt 2 False, obsAt 3 True]
          -- SQLite assigns ids 1..3 in insert order.
          _ <- Db.correctObservation h 1 (ToPet (PetId "mochi"))
          _ <- Db.correctObservation h 3 ToVisiting
          let lo = base
              hi = addUTCTime 100000 base
          obss <- Db.observationsBetween h lo hi
          ov <- Db.overridesBetween h lo hi
          sqlStats <- Db.subjectStatsBetween h lo hi
          let blob = presence ov twoCat obss
              petOrSpecies k = case k of
                KPet _     -> True
                KSpecies _ -> True
                _          -> False
          -- The SQL aggregate excludes persons/visitors, so compare on the
          -- pet/species buckets: the two paths must agree exactly.
          Map.filterWithKey (\k _ -> petOrSpecies k) blob @?= sqlStats
          -- The specifics: obs 1 credited to Mochi (ate), obs 2 stays species
          -- (ambiguous cat), obs 3 (visitor) excluded from both pet and species.
          fmap psAte (Map.lookup (KPet (PetId "mochi")) sqlStats) @?= Just 1
          Map.lookup (KPet (PetId "miso")) sqlStats @?= Nothing
          fmap psSightings (Map.lookup (KSpecies (Species "cat")) sqlStats) @?= Just 1
          fmap psSightings (Map.lookup (KVisitor (Species "cat")) blob) @?= Just 1
  , testCase "the daily rollup materialises the same aggregate a live window computes" $
      withSystemTempDirectory "petreport-rollup" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              tk k = addUTCTime (fromInteger (k * 100)) base
              catAp eat =
                Appearance
                  (AnAnimal (Species "cat"))
                  (if eat then Eating else Sleeping)
                  (normalizeBehaviors (if eat then Eating else Sleeping) noBehaviors)
                  Nothing
              obsAt k eat =
                NewObservation (tk k) (Camera "office") PeriodicSample (Seen emptyScene {appearances = [catAp eat]})
              -- A DOG appearance corrected to the (cat) pet Mochi, so Mochi spans two
              -- detected species: the rollup must SUM both groups into KPet, not drop one.
              dogAp = Appearance (AnAnimal (Species "dog")) Sleeping (normalizeBehaviors Sleeping noBehaviors) Nothing
              obsDog k =
                NewObservation (tk k) (Camera "office") PeriodicSample (Seen emptyScene {appearances = [dogAp]})
              lo = base
              hi = addUTCTime 100000 base
              day = "2026-07-08"
          mapM_ (Db.insertObservation h) [obsAt 1 True, obsAt 2 False, obsAt 3 True, obsDog 4]
          _ <- Db.correctObservation h 1 (ToPet (PetId "mochi"))
          -- The rollup for the day equals a live compute over the same window.
          live <- Db.subjectStatsBetween h lo hi
          Db.materializeDay h day lo hi
          Db.dailyPetStats h day >>= (@?= live)
          -- Re-materialising after further corrections replaces the day cleanly, never
          -- doubling, and Mochi (now seen as both cat and dog) sums across both species.
          _ <- Db.correctObservation h 2 (ToPet (PetId "mochi"))
          _ <- Db.correctObservation h 4 (ToPet (PetId "mochi"))
          live2 <- Db.subjectStatsBetween h lo hi
          Db.materializeDay h day lo hi
          rolled2 <- Db.dailyPetStats h day
          rolled2 @?= live2
          fmap psSightings (Map.lookup (KPet (PetId "mochi")) rolled2) @?= Just 3
  , testCase "GC collects un-kept moments in a window but spares the kept one" $
      withSystemTempDirectory "petreport-gc" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              tk k = addUTCTime (fromInteger (k * 100)) base
              catAp =
                Appearance (AnAnimal (Species "cat")) Sleeping (normalizeBehaviors Sleeping noBehaviors) Nothing
              obsAt k = NewObservation (tk k) (Camera "office") PeriodicSample (Seen emptyScene {appearances = [catAp]})
              lo = base
              hi = addUTCTime 100000 base
          mapM_ (Db.insertObservation h) [obsAt 1, obsAt 2, obsAt 3]
          -- Keep the middle moment (SQLite assigns ids 1..3 in insert order).
          _ <- Db.insertKeepsake h 2 Nothing Nothing base
          -- The day has un-kept moments to collect, so GC would process it.
          Db.hasUnkeptBetween h lo hi >>= (@?= True)
          -- Materialise the FULL day, then collect: the two un-kept go, the kept stays.
          Db.materializeDay h "2026-07-08" lo hi
          Db.collectUnkept h lo hi >>= (@?= 2)
          remaining <- Db.observationsBetween h lo hi
          [i | o <- remaining, let ObsId i = obsId o] @?= [2]
          -- After collection the day has no un-kept moments left, so GC skips it on the
          -- next batch and never re-materialises its rollup from the now-kept-only day
          -- (which would shrink the full-day stats). The full-day rollup (3 sightings)
          -- is preserved.
          Db.hasUnkeptBetween h lo hi >>= (@?= False)
          rolled <- Db.dailyPetStats h "2026-07-08"
          fmap psSightings (Map.lookup (KSpecies (Species "cat")) rolled) @?= Just 3
  , -- The SINGLE-pet reconciliation in 'petStatFor' (finding 4): the SQL aggregate
    -- keys un-overridden sightings of a lone cat by KSpecies (it holds no roster
    -- rule), while the blob path resolves them straight to KPet via the roster's
    -- unique-species rule. 'petStatFor' bridges the gap by folding KSpecies into
    -- KPet for the unique pet of its species, so the two paths agree even with no
    -- override remap exercised. The two-cat test above never hits this fold (two
    -- cats => no unique species), so pin it separately; it guards a future switch
    -- of petsAtH to the SQL aggregate.
    testCase "petStatFor agrees with blob presence for a lone pet of its species" $
      withSystemTempDirectory "petreport-lone" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let dexter = Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing
              roster = [dexter]
              base = UTCTime (fromGregorian 2026 7 8) 0
              tk k = addUTCTime (fromInteger (k * 100)) base
              catAp act =
                Appearance
                  (AnAnimal (Species "cat"))
                  act
                  (normalizeBehaviors act noBehaviors)
                  Nothing
              obsAt k act =
                NewObservation
                  (tk k)
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [catAp act]})
          -- A mix of un-overridden cat sightings plus one explicit override to the
          -- pet: obs 1 eats, obs 2 sleeps, obs 3 drinks.
          mapM_ (Db.insertObservation h) [obsAt 1 Eating, obsAt 2 Sleeping, obsAt 3 Drinking]
          -- SQLite assigns ids 1..3 in insert order; correct obs 1 to the pet so the
          -- SQL path yields a KPet bucket while obs 2/3 stay in KSpecies.
          _ <- Db.correctObservation h 1 (ToPet (PetId "dexter"))
          let lo = base
              hi = addUTCTime 100000 base
          obss <- Db.observationsBetween h lo hi
          ov <- Db.overridesBetween h lo hi
          sqlStats <- Db.subjectStatsBetween h lo hi
          -- The SQL path splits the lone cat across KPet (override) and KSpecies
          -- (un-overridden); petStatFor must fold both into the pet's stat.
          fmap psSightings (Map.lookup (KPet (PetId "dexter")) sqlStats) @?= Just 1
          fmap psSightings (Map.lookup (KSpecies (Species "cat")) sqlStats) @?= Just 2
          let blob = presence ov roster obss
          -- The blob path resolves EVERY sighting to KPet via the unique-species rule.
          fmap psSightings (Map.lookup (KPet (PetId "dexter")) blob) @?= Just 3
          -- The invariant: petStatFor over the SQL aggregate equals the blob KPet bucket.
          petStatFor roster dexter sqlStats @?= Map.lookup (KPet (PetId "dexter")) blob
  , testCase "reprojectAll preserves overrides; a person correction clears them" $
      withSystemTempDirectory "petreport-spec2" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              catObs =
                NewObservation
                  base
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
          Db.insertObservation h catObs
          _ <- Db.correctObservation h 1 (ToPet (PetId "mochi"))
          let lo = addUTCTime (-100) base
              hi = addUTCTime 100 base
          before <- Db.overridesBetween h lo hi
          _ <- Db.reprojectAll h
          after <- Db.overridesBetween h lo hi
          -- Re-projecting the facts table must not touch the override table.
          after @?= before
          Map.lookup (1, 0) after @?= Just (IdPet (PetId "mochi"))
          -- Correcting to a person clears the pet override for that observation.
          _ <- Db.correctObservation h 1 ToPerson
          cleared <- Db.overridesBetween h lo hi
          Map.lookup (1, 0) cleared @?= Nothing
  , testCase "purgePetAndProfile erases the pet's data and drops it from the roster in one call" $
      withSystemTempDirectory "petreport-purge" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              hi = addUTCTime 100000 base
              miso = Pet (PetId "miso") "Miso" (Species "cat") "grey cat" Nothing Nothing Nothing
              mochi = Pet (PetId "mochi") "Mochi" (Species "cat") "black cat" Nothing Nothing Nothing
              catObs k =
                NewObservation
                  (addUTCTime (fromInteger (k * 100)) base)
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
          -- Seed a two-pet roster and mochi's cached summary.
          Db.putProfile h emptyProfile {pets = [miso, mochi]}
          Db.putPetSummary h "mochi" base (Db.PetSummary Settled (Just "a good week") [])
          mapM_ (Db.insertObservation h) [catObs 1, catObs 2, catObs 3, catObs 4]
          _ <- Db.correctObservation h 1 (ToPet (PetId "mochi")) -- mochi's moment
          _ <- Db.correctObservation h 2 (ToPet (PetId "miso")) -- miso's moment
          _ <- Db.correctObservation h 3 (ToPet (PetId "miso")) -- miso's, but was kept as mochi
          _ <- Db.insertKeepsake h 3 (Just "mochi") Nothing base
          _ <- Db.insertKeepsake h 4 (Just "mochi") Nothing base -- kept as mochi, never corrected
          deleted <-
            Db.purgePetAndProfile h "mochi" $ \p ->
              p {pets = filter ((/= PetId "mochi") . petId) (pets p)}
          -- Deleted: mochi's corrected moment (1) and its kept-but-uncorrected one (4).
          -- Kept: obs 3 (since re-corrected to miso) and obs 2 (miso's).
          sort [i | o <- deleted, let ObsId i = obsId o] @?= [1, 4]
          survivors <- Db.observationsBetween h base hi
          sort [i | o <- survivors, let ObsId i = obsId o] @?= [2, 3]
          ov <- Db.overridesBetween h base hi
          Map.lookup (1, 0) ov @?= Nothing
          Map.lookup (2, 0) ov @?= Just (IdPet (PetId "miso"))
          Map.lookup (3, 0) ov @?= Just (IdPet (PetId "miso"))
          -- All of mochi's keepsakes gone (incl. the stale one on obs 3).
          ks <- Db.listKeepsakes h Nothing
          length ks @?= 0
          -- mochi's cached summary is gone; and the same call dropped it from the roster.
          Db.getPetSummary h "mochi" >>= (@?= Nothing)
          prof <- Db.getProfile h
          sort [p | Pet {petId = PetId p} <- pets prof] @?= ["miso"]
  , testCase "browseMoments facets, keyset-pages, and honours sort direction" $
      withSystemTempDirectory "petreport-browse" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              at k = addUTCTime (fromInteger (k * 100)) base
              cat act conf k =
                NewObservation
                  (at k)
                  (Camera "office")
                  PeriodicSample
                  ( Seen
                      emptyScene
                        { appearances = [Appearance (AnAnimal (Species "cat")) act noBehaviors Nothing]
                        , confidence = Just (mkConfidence conf)
                        }
                  )
          -- obs 1,3 sleeping (certain); obs 2 playing (certain); obs 4 sleeping but
          -- low-confidence, so uncertain and unreviewed (the needs-look backlog).
          mapM_
            (Db.insertObservation h)
            [cat Sleeping 0.9 1, cat Playing 0.9 2, cat Sleeping 0.9 3, cat Sleeping 0.4 4]
          let bq0 =
                Db.BrowseQuery
                  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                  Db.Asc Nothing 50
              ids p = [i | o <- Db.bpItems p, let ObsId i = obsId o]
          -- Activity facet resolves to the projected activity column.
          play <- Db.browseMoments h bq0 {Db.bqActivity = Just "playing"}
          ids play @?= [2]
          -- Needs-look facet: only the uncertain, unreviewed moment.
          nl <- Db.browseMoments h bq0 {Db.bqReview = Just Db.NeedsLook}
          ids nl @?= [4]
          -- The keyset cursor pages the whole set with no gap or overlap, and the
          -- total is reported on the first page only.
          pg1 <- Db.browseMoments h bq0 {Db.bqLimit = 2}
          ids pg1 @?= [1, 2]
          Db.bpTotal pg1 @?= Just 4
          pg2 <- Db.browseMoments h bq0 {Db.bqLimit = 2, Db.bqCursor = Db.bpNextCursor pg1}
          ids pg2 @?= [3, 4]
          Db.bpNextCursor pg2 @?= Nothing
          Db.bpTotal pg2 @?= Nothing
          -- Sort direction reverses the order.
          desc <- Db.browseMoments h bq0 {Db.bqSort = Db.Desc}
          ids desc @?= [4, 3, 2, 1]
          -- Pet facet mirrors the stats attribution. Correct obs 2 to a specific pet;
          -- the unique-active-species branch then counts every unattributed cat too,
          -- while the no-species branch counts only the explicit override.
          _ <- Db.correctObservation h 2 (ToPet (PetId "mochi"))
          allCats <- Db.browseMoments h bq0 {Db.bqPet = Just (Db.PetFilter "mochi" (Just "cat"))}
          ids allCats @?= [1, 2, 3, 4]
          onlyMochi <- Db.browseMoments h bq0 {Db.bqPet = Just (Db.PetFilter "mochi" Nothing)}
          ids onlyMochi @?= [2]
          -- Free-text search over the perception blob: every cat matches "cat", none a miss.
          hits <- Db.browseMoments h bq0 {Db.bqSearch = Just "cat"}
          ids hits @?= [1, 2, 3, 4]
          misses <- Db.browseMoments h bq0 {Db.bqSearch = Just "zebra"}
          ids misses @?= []
          -- Time-of-day buckets by LOCAL wall-clock, so the tz offset is ADDED to the
          -- UTC timestamp. These UTC-midnight moments fall in the morning window under
          -- a +6h offset; they would land in the evening if the offset were subtracted.
          morning <- Db.browseMoments h bq0 {Db.bqTimeOfDay = Just (Db.TimeBucket (5 * 3600) (12 * 3600), 6 * 3600)}
          ids morning @?= [1, 2, 3, 4]
          night <- Db.browseMoments h bq0 {Db.bqTimeOfDay = Just (Db.TimeBucket 0 (5 * 3600), 6 * 3600)}
          ids night @?= []
  , testCase "browse media facet keys off perception kind, not subject rows" $
      withSystemTempDirectory "petreport-media" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              at k = addUTCTime (fromInteger (k * 100)) base
          -- 1: a scene the model returned empty (nothing identifiable) writes no
          --    subject rows, yet it is still a photo, so it must NOT read as audio.
          Db.insertObservation h (NewObservation (at 1) (Camera "office") PeriodicSample (Seen emptyScene))
          -- 2: an ordinary scene sample, a photo.
          Db.insertObservation
            h
            ( NewObservation
                (at 2)
                (Camera "office")
                PeriodicSample
                (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
            )
          -- 3: a sound, which is audio whether or not it has any subject rows.
          Db.insertObservation h (NewObservation (at 3) (Camera "office") PeriodicSample (Heard (SoundKind "bark")))
          let bq0 =
                Db.BrowseQuery
                  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                  Db.Asc Nothing 50
              ids p = [i | o <- Db.bpItems p, let ObsId i = obsId o]
          audio <- Db.browseMoments h bq0 {Db.bqMedia = Just Db.MediaAudio}
          ids audio @?= [3]
          photo <- Db.browseMoments h bq0 {Db.bqMedia = Just Db.MediaPhoto}
          ids photo @?= [1, 2]
  , testCase "deleteObservation cascades to the keepsake via the FK" $
      withSystemTempDirectory "petreport-cascade" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              catObs =
                NewObservation
                  base
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
          Db.insertObservation h catObs
          _ <- Db.insertKeepsake h 1 (Just "mochi") (Just "nap") base
          Db.listKeepsakes h Nothing >>= ((@?= 1) . length)
          -- No manual keepsake delete: the FK's ON DELETE CASCADE must take it.
          _ <- Db.deleteObservation h 1
          Db.listKeepsakes h Nothing >>= ((@?= 0) . length)
  , testCase "insertKeepsake is idempotent per moment (re-Keeping makes no second row)" $
      withSystemTempDirectory "petreport-keepsake-idem" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              catObs =
                NewObservation
                  base
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
          Db.insertObservation h catObs
          -- Keep the same moment twice (the Lightbox re-Keep after navigating away).
          first <- Db.insertKeepsake h 1 (Just "mochi") (Just "nap") base
          second <- Db.insertKeepsake h 1 (Just "mochi") (Just "later caption") base
          -- Exactly one row, and the second call returns the same (first) keepsake:
          -- same id, and the FIRST caption survives (ON CONFLICT DO NOTHING).
          Db.listKeepsakes h Nothing >>= ((@?= 1) . length)
          Db.kId second @?= Db.kId first
          Db.kCaption second @?= Just "nap"
          -- Delete by that id still removes the single keepsake, returning the
          -- moment id it referenced so the caller can free that moment's owned media.
          Db.deleteKeepsake h (Db.kId first) >>= (@?= Just (Db.kObsId first))
          Db.listKeepsakes h Nothing >>= ((@?= 0) . length)
  , testCase "getObservationsByIds batches the single-row reads and agrees with them" $
      withSystemTempDirectory "petreport-batch" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              catObs k =
                NewObservation
                  (addUTCTime (fromInteger (k * 100)) base)
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
          mapM_ (Db.insertObservation h) [catObs 1, catObs 2, catObs 3]
          _ <- Db.correctObservation h 2 (ToPet (PetId "mochi"))
          -- The batch read is keyed by id and holds exactly the ids that exist; a
          -- missing id (99) is simply absent, as the single-row Nothing would drop it.
          byId <- Db.getObservationsByIds h [1, 2, 3, 99]
          sort (Map.keys byId) @?= [1, 2, 3]
          -- Each batched row equals the single-row path (so the enrichment is unchanged).
          single <- mapM (Db.getObservation h) [1, 2, 3]
          map (Just . snd) (Map.toAscList byId) @?= single
          -- An empty id list short-circuits to no query and an empty map.
          Db.getObservationsByIds h [] >>= ((@?= []) . Map.keys)
          -- The batched overrides equal the union of the per-obs overrides.
          batchedOv <- Db.overridesForObsIds h [1, 2, 3]
          perObs <- mconcat <$> mapM (Db.overridesForObs h) [1, 2, 3]
          batchedOv @?= perObs
          Map.lookup (2, 0) batchedOv @?= Just (IdPet (PetId "mochi"))
          Db.overridesForObsIds h [] >>= ((@?= []) . Map.keys)
  , testCase "markReviewed marks every id in the batch" $
      withSystemTempDirectory "petreport-reviewed" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $ \h -> do
          let base = UTCTime (fromGregorian 2026 7 8) 0
              hi = addUTCTime 100000 base
              catObs k =
                NewObservation
                  (addUTCTime (fromInteger (k * 100)) base)
                  (Camera "office")
                  PeriodicSample
                  (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]})
          mapM_ (Db.insertObservation h) [catObs 1, catObs 2, catObs 3]
          Db.markReviewed h [1, 2, 3]
          obss <- Db.observationsBetween h base hi
          sort [i | Observation {obsId = ObsId i, reviewed = r} <- obss, r] @?= [1, 2, 3]
  , testCase "a fresh DB migrates to the latest schema version" $
      withSystemTempDirectory "petreport-migrate" $ \dir ->
        Db.withHandle (dbTracer renderingTracer) (dir </> "spec.db") $
          Db.schemaVersion >=> (@?= Db.latestSchemaVersion)
  , testCase "a DB newer than this build's schema is refused, not run against" $ do
      -- The pre-reset (v11) DB met by the greenfield v1 schema: startup must fail fast
      -- with an actionable message, not 500 per query on a column the old schema lacks.
      isJust (Migrations.schemaTooNew (Db.latestSchemaVersion + 1)) @?= True
      Migrations.schemaTooNew Db.latestSchemaVersion @?= Nothing
      Migrations.schemaTooNew 0 @?= Nothing
  , testCase "a populated DB carrying no schema version is refused, not built over" $ do
      -- SQLite reports an unstamped database as version 0, exactly like a fresh file, so
      -- the number alone cannot tell them apart. Building v1 over a populated one dies on
      -- its first CREATE TABLE with a raw SQLite error; it must fail fast with a fix.
      Migrations.unversionedButPopulated [] @?= Nothing
      -- An unrelated file: name what was found and what was expected, and say to move it
      -- aside. Stamping this one would only trade a clear startup error for an opaque
      -- "no such table" on the first query.
      case Migrations.unversionedButPopulated ["observations", "profile"] of
        Nothing -> assertFailure "expected a refusal for a populated unstamped database"
        Just m -> do
          let msg = pack m
          ("observations, profile" `isInfixOf` msg) @?= True
          ("Move the file aside" `isInfixOf` msg) @?= True
          ("PRAGMA user_version" `isInfixOf` msg) @?= False
      -- The full v1 table set with no stamp is what a .dump-and-restore of a healthy
      -- database produces. That operator must be told to restore the stamp, never to
      -- delete: the data is intact.
      case Migrations.unversionedButPopulated Migrations.schemaV1Tables of
        Nothing -> assertFailure "expected a refusal for an unstamped v1 database"
        Just m -> do
          let msg = pack m
          ("PRAGMA user_version = 1" `isInfixOf` msg) @?= True
          ("Do not delete" `isInfixOf` msg) @?= True
      -- The set is recovered from the DDL, so it cannot drift from what migrate applies.
      assertBool "v1 creates several tables" (length Migrations.schemaV1Tables >= 5)
      ("observations" `elem` Migrations.schemaV1Tables) @?= True
  ]
