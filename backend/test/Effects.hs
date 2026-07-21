-- | Coverage for the I/O boundaries the effect handles are records-of-functions to make
-- testable: the Frigate decoders against real 0.18 fixtures, the Llm client's pure request
-- shaping, the Agent.ask loop, the Pipeline event ingest, and a handful of Web handlers.
--
-- Everything runs through fake handles over a real temporary SQLite database, so no test
-- ever touches the network or a model.
module Effects
  ( effectGroups
  ) where

import           Control.Concurrent.MVar (newMVar)
import           Control.Concurrent.STM  (atomically, newTVarIO, writeTVar)
import           Control.Monad            (void)
import           Data.List               (sort, sortOn)
import           Data.Maybe              (mapMaybe)
import           Data.Ord                (Down (..))
import           Control.Exception       (IOException, SomeException, throwIO,
                                          try)
import           Data.Aeson              (FromJSON, Value (..),
                                          decodeFileStrict, decode, encode,
                                          object, (.=))
import qualified Data.Aeson.Key          as Key
import qualified Data.Aeson.KeyMap       as KeyMap
import qualified Data.ByteString.Lazy    as LBS
import           Data.IORef              (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict         as Map
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Time               (UTCTime (..), fromGregorian)
import           Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)

import           Hedgehog            (Property, forAll, property, (===))
import qualified Hedgehog.Gen        as Gen
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.HUnit    (assertBool, assertFailure,
                                      testCase, (@?=))

import           Servant         (ServerError (..), getResponse,
                                  runHandler)
import           System.FilePath ((</>))
import           System.IO.Temp  (withSystemTempDirectory)

import           LLM.Agent       (renderResult)
import           LLM.Call        (Call (..), Sampling (..), runRepair, structured,
                                  userText)

import           PetReport.App                (App (..), EffSettings (..),
                                               Runtime (..), appSettings)
import           PetReport.Config             (Config (..), mkHour, parseListen,
                                               portNumber)
import           PetReport.Domain.Behavior    (noBehaviors)
import           PetReport.Domain.Observation (FrigateMeta (..),
                                               NewObservation (..),
                                               Observation (..), Origin (..))
import           PetReport.Domain.Perception  (Appearance (..), Perception (..),
                                               Scene (..), Who (..), emptyScene)
import           PetReport.Domain.Profile     (CameraRoom (..), Pet (..),
                                               Profile (..), emptyProfile)
import           PetReport.Domain.Types       (Activity (..), BaseUrl (..),
                                               Camera (..), EventId (..),
                                               ModelName (..), ObsId (..),
                                               PetId (..), Species (..))
import qualified PetReport.Analysis.Agent     as Agent
import qualified PetReport.Effect.Clock       as Clock
import qualified PetReport.Effect.Db          as Db
import qualified PetReport.Effect.Ffmpeg      as Ffmpeg
import           PetReport.Effect.Frigate     (FrigateConfig (..),
                                               FrigateEvent (..),
                                               FrigateStats (..), MediaKind (..),
                                               Transcript (..), eventMedia)
import qualified PetReport.Effect.Frigate     as Frigate
import           PetReport.Effect.Llm         (ChatRequest (..),
                                               AssistantMessage (..),
                                               ToolCall (..), ToolSpec (..),
                                               defaultRequest, requestJson)
import qualified PetReport.Effect.Llm         as Llm
import qualified PetReport.Effect.Ntfy        as Ntfy
import qualified PetReport.Pipeline           as Pipeline
import           PetReport.Trace              (dbTracer, ntfyTracer,
                                               renderingTracer)
import qualified PetReport.Web                as Web
import           PetReport.Web.Ops            (askH)
import           PetReport.Web.Types          (AskReq (..))
import           PetReport.Pipeline.Worker    (newJobs)

-- The effect-seam groups, hung off the top-level tree in Main.
effectGroups :: [TestTree]
effectGroups =
  [ testGroup "frigate decoders" frigateUnits
  , testGroup "llm client" llmUnits
  , testGroup "agent" agentUnits
  , testGroup "pipeline ingest" pipelineUnits
  , testGroup "web handlers" webUnits
  , testGroup "api contract (handlers)" contractHandlerUnits
  , testGroup "config" configUnits
  ]

-- --------------------------------------------------------------------------- --
-- (a) Frigate decoders
-- --------------------------------------------------------------------------- --

-- The fixtures are real Frigate 0.18 responses, and the decoders read only the slice the
-- effect uses. So pin the content of each that matters, plus the total rules of 'eventMedia'
-- and 'transcriptFrom' that ingest hangs on.
frigateUnits :: [TestTree]
frigateUnits =
  [ testCase "config decodes the camera names and the transcription flag" $ do
      cfg <- decodeFixture "config.json" :: IO FrigateConfig
      -- The keys of config.cameras, every camera Frigate has configured.
      fcCameraNames cfg @?= ["hallway", "kitchen", "living_room", "office"]
      -- audio_transcription.enabled is False in this capture.
      fcTranscription cfg @?= False
  , testCase "stats decodes the per-camera fps map, keyed by camera" $ do
      st <- decodeFixture "stats.json" :: IO FrigateStats
      let fps = fsCameraFps st
      -- Every configured camera contributes a key; only office is live.
      Map.keys fps @?= ["hallway", "kitchen", "living_room", "office"]
      Map.lookup "office" fps @?= Just 5.3
      Map.lookup "hallway" fps @?= Just 0
  , testCase "events decodes the list, its labels, cameras, and media flags" $ do
      evs <- decodeFixture "events.json" :: IO [FrigateEvent]
      length evs @?= 3
      map feCamera evs @?= ["office", "office", "office"]
      map feLabel evs @?= ["person", "speech", "person"]
      -- Every event in this capture is completed (end_time set) with clip + snapshot.
      all (\e -> feHasClip e && feHasSnapshot e) evs @?= True
      map (\e -> case feEnd e of Just _ -> True; Nothing -> False) evs @?= [True, True, True]
      -- The score is pulled from data.top_score (there is no top-level score).
      case evs of
        (e0 : _) -> assertBool "first event's top_score is read" (feScore e0 > 0.89)
        []       -> assertFailure "events fixture decoded empty"
  , testGroup "transcript tolerance" transcriptUnits
  , testProperty "eventMedia resolves clip/snapshot/none from the flags" prop_eventMedia
  ]

-- The five shapes 'Transcript' tolerates across Frigate versions, plus the rule that a blank
-- collapses to 'Nothing'.
transcriptUnits :: [TestTree]
transcriptUnits =
  [ testCase "top-level transcription" $ transOf ["transcription" .= t] @?= Just "hello there"
  , testCase "top-level text" $ transOf ["text" .= t] @?= Just "hello there"
  , testCase "top-level transcript" $ transOf ["transcript" .= t] @?= Just "hello there"
  , testCase "nested under data.transcription" $
      transOf ["data" .= object ["transcription" .= t]] @?= Just "hello there"
  , testCase "nested under data.text" $
      transOf ["data" .= object ["text" .= t]] @?= Just "hello there"
  , testCase "a blank result collapses to Nothing" $
      transOf ["transcription" .= ("   " :: Text)] @?= Nothing
  ]
  where
    t = "  hello there  " :: Text -- also exercises the strip

prop_eventMedia :: Property
prop_eventMedia = property $ do
  clip <- forAll Gen.bool
  snap <- forAll Gen.bool
  let ev = mkEvent {feHasClip = clip, feHasSnapshot = snap}
      expected
        | clip      = HasClip
        | snap      = SnapshotOnly
        | otherwise = NoMedia
  eventMedia ev === expected

-- | Decode a Transcript from a top-level object, mirroring the transcribe path, then unwrap
-- it. A well-formed object always decodes, so a failure here is a bug.
transOf :: [(Key.Key, Value)] -> Maybe Text
transOf kvs = case decode (encode (object kvs)) :: Maybe Transcript of
  Just tr -> unTranscript tr
  Nothing -> error "transcript object failed to decode"

-- --------------------------------------------------------------------------- --
-- (b) Llm client
-- --------------------------------------------------------------------------- --

-- The wire request the client builds is pure, so pin the fields the llama.cpp server needs
-- and the conditional inclusion of tools and response_format. The retry cannot be reached
-- without an HTTP call, so it belongs to the integration boundary; only the pure request
-- shaping is covered here.
llmUnits :: [TestTree]
llmUnits =
  [ testCase "retry backoff is monotonic and capped at 8s" $ do
      -- With reqRetries = 2 the schedule reads 2s then 4s. Deeper retry budgets saturate
      -- at the 8s cap, so the serial worker is never stalled for long.
      map (Llm.retryBackoffMicros 2) [2, 1] @?= [2000000, 4000000]
      map (Llm.retryBackoffMicros 5) [5, 4, 3, 2, 1] @?= [2000000, 4000000, 8000000, 8000000, 8000000]
  , testCase "model and the base fields are always present" $ do
      let o = objOf (requestJson "qwen-vl" defaultRequest {reqTemperature = 0.25})
      KeyMap.lookup "model" o @?= Just (String "qwen-vl")
      KeyMap.lookup "temperature" o @?= Just (Number 0.25)
      hasKey "messages" o @?= True
      hasKey "max_tokens" o @?= True
  , testCase "tools appear iff tools are set (and tool_choice with them)" $ do
      let without = objOf (requestJson "m" defaultRequest)
          withT = objOf (requestJson "m" defaultRequest {reqTools = [oneTool]})
      hasKey "tools" without @?= False
      hasKey "tool_choice" without @?= False
      hasKey "tools" withT @?= True
      KeyMap.lookup "tool_choice" withT @?= Just (String "auto")
  , testCase "response_format appears iff a schema is set" $ do
      let without = objOf (requestJson "m" defaultRequest)
          withRf = objOf (requestJson "m" defaultRequest {reqResponseFormat = Just (String "sch")})
      hasKey "response_format" without @?= False
      KeyMap.lookup "response_format" withRf @?= Just (String "sch")
  , testCase "repair recovers: a malformed reply is retried with the error fed back" $ do
      -- The model answers with prose first (no usable JSON), then a codec-valid Scene.
      -- runRepair feeds the decode error back and the second attempt decodes, so the bad
      -- reply is repaired rather than surfacing as a failure.
      calls <- newIORef (0 :: Int)
      let handle = fakeLlm $ \_ -> do
            n <- readIORef calls
            modifyIORef' calls (+ 1)
            pure (answer (if n == 0 then "sorry, I can't answer that" else sceneReply))
      result <- runRepair handle 1 sceneProbe
      n <- readIORef calls
      case result of
        Right _ -> n @?= 2 -- the bad reply, then the corrected one
        Left e  -> assertFailure ("repair did not recover: " <> show e)
  , testCase "repair gives up after the retry budget on a persistently bad model" $ do
      -- A model that never returns usable JSON is retried exactly the budgeted number of
      -- times (initial attempt plus two), then the loop stops with the decode error.
      calls <- newIORef (0 :: Int)
      let handle = fakeLlm $ \_ -> modifyIORef' calls (+ 1) >> pure (answer "still not json")
      result <- runRepair handle 2 sceneProbe
      n <- readIORef calls
      either (const True) (const False) result @?= True
      n @?= 3
  , testCase "renderResult passes a small result through and refuses an oversize one" $ do
      let small = object ["ok" .= True]
          big = object ["items" .= replicate 1000 ("observation text here" :: Text)]
      assertBool "small passes through" (not ("tool_result_too_large" `T.isInfixOf` renderResult 100000 small))
      assertBool "small keeps its content" ("ok" `T.isInfixOf` renderResult 100000 small)
      assertBool "oversize is refused" ("tool_result_too_large" `T.isInfixOf` renderResult 500 big)
      assertBool "oversize drops the payload" (not ("observation text here" `T.isInfixOf` renderResult 500 big))
  ]
  where
    oneTool = ToolSpec "activity_stats" "counts" (object ["type" .= ("object" :: Text)])
    sceneProbe :: Call Scene
    sceneProbe =
      Call
        { callMessages = [userText "describe the scene"]
        , callSampling = Sampling 0.1 200 False
        , callBudget = Llm.interactiveBudget
        , callReply = structured @Scene "scene"
        }

-- --------------------------------------------------------------------------- --
-- (c) Agent
-- --------------------------------------------------------------------------- --

-- The ask loop over a fake Llm: a direct answer passes through, a tool-call turn
-- runs the tool (against the real temp DB) then the model's final answer, and a
-- thrown HttpException-like fault surfaces as an error string, not a crash.
agentUnits :: [TestTree]
agentUnits =
  [ testCase "a direct answer passes through" $
      withFakeApp (\a -> a {appLlm = fakeLlm (const (pure (answer "Dexter napped all day.")))}) $ \app -> do
        (ans, ids) <- runAsk app "how was Dexter today?"
        ans @?= "Dexter napped all day."
        -- A direct answer runs no moment tool, so it surfaces nothing.
        ids @?= []
  , testCase "a tool-call turn is executed, then the final answer is returned" $
      withFakeApp (\a -> a {appLlm = fakeLlm scriptedChat}) $ \app -> do
        (ans, ids) <- runAsk app "how many observations today?"
        ans @?= "I found the count."
        -- count_observations is an aggregate tool, so it surfaces no moments.
        ids @?= []
  , testCase "a moment tool surfaces the ids of the observations it read" $
      -- daily_timeline renders per-observation lines; those exact ids come back so
      -- the caller can enrich them into cards. Insert two of today's observations,
      -- script a single daily_timeline call, and pin the surfaced ids against what
      -- the DB actually holds (most-recent-first).
      withFakeApp (\a -> a {appLlm = fakeLlm timelineThenAnswer}) $ \app -> do
        stored <- seedTodayObservations app
        (ans, ids) <- runAsk app "what happened today?"
        ans @?= "Here is the timeline."
        ids @?= sortOn Down stored
  , testCase "a thrown model fault surfaces as an error, not a crash" $
      withFakeApp (\a -> a {appLlm = fakeLlm (const (throwIO (userError "connection refused" :: IOException)))}) $ \app -> do
        r <- tryAny (runAsk app "anything?")
        -- ask itself does not catch the chat failure; it is the caller (askH) that
        -- would. Pin the actual behaviour: the exception propagates as an
        -- IOException rather than the loop hanging or corrupting state.
        case r of
          Left e  -> assertBool "the model fault propagates" ("connection refused" `T.isInfixOf` T.pack (show e))
          Right _ -> assertFailure "expected the model fault to propagate"
  , testCase "askH turns a model outage into a 503, not a 500" $
      -- The complement of the previous case: the ask handler catches the propagated
      -- fault ('orUnavailable') and answers 503 Unavailable rather than warp's 500.
      withFakeApp (\a -> a {appLlm = fakeLlm (const (throwIO (userError "connection refused" :: IOException)))}) $ \app -> do
        res <- runHandler (askH app (AskReq "how is Dexter?"))
        case res of
          Left err -> errHTTPCode err @?= 503
          Right _  -> assertFailure "expected a 503 (model unavailable), got a response"
  , testCase "a model that never answers terminates on the iteration bound" $
      -- The wall-clock deadline is belt-and-suspenders and hard to test against a
      -- real clock without a flaky timing assertion, so pin the secondary bound: a
      -- model that always asks for another tool still terminates (via maxIters),
      -- returning the honest can't-tell fallback rather than looping forever.
      withFakeApp (\a -> a {appLlm = fakeLlm (const (pure (toolTurn "count_observations")))}) $ \app -> do
        (ans, _) <- runAsk app "loop forever?"
        ans @?= "I could not find an answer in the footage."
  ]
  where
    -- Turn 1: the model asks to run count_observations. Turn 2 (after the tool
    -- result is appended): a plain answer. The dispatch runs against the real DB.
    scriptedChat req
      | any ((== "tool") . roleOf) (reqMessages req) = pure (answer "I found the count.")
      | otherwise = pure (toolTurn "count_observations")
    -- The same two-turn shape, but the moment tool: daily_timeline surfaces ids.
    timelineThenAnswer req
      | any ((== "tool") . roleOf) (reqMessages req) = pure (answer "Here is the timeline.")
      | otherwise = pure (toolTurn "daily_timeline")

-- | Drive 'Agent.ask' with the profile/now/tz the web layer would hand it, read
-- from the fake app's own DB and clock (the agent no longer re-reads them).
runAsk :: App -> Text -> IO (Text, [ObsId])
runAsk app q = do
  now <- Clock.now (appClock app)
  tz <- Clock.timeZone (appClock app)
  prof <- Db.getProfile (appDb app)
  Agent.ask app prof now tz q

-- | Insert two observations dated at the fake clock's "now" so they fall inside
-- today's window, then return their DB-assigned ids (the INSERT ignores the id on
-- the value, so read them back). The agent's daily_timeline tool surfaces these.
seedTodayObservations :: App -> IO [ObsId]
seedTodayObservations app = do
  now <- Clock.now (appClock app)
  let sc = emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]}
      obs t = NewObservation t (Camera "office") PeriodicSample (Seen sc)
  Db.insertObservation (appDb app) (obs (addSecs (-120) now))
  Db.insertObservation (appDb app) (obs (addSecs (-60) now))
  map obsId <$> Db.observationsBetween (appDb app) (addSecs (-3600) now) (addSecs 3600 now)

-- --------------------------------------------------------------------------- --
-- (d) Pipeline ingest
-- --------------------------------------------------------------------------- --

-- Event ingest driven through a fake Frigate handle over a real temp DB. Covers
-- the audio path, the fetch-failure hold, the NoMedia completed/in-progress split,
-- and the snapshot-only vision path (with a codec-valid Scene reply).
pipelineUnits :: [TestTree]
pipelineUnits =
  [ testCase "an audio-label event becomes a sound observation" $
      withFakeApp (\a -> a {appFrigate = fakeFrigate {Frigate.recentEvents = \_ _ _ _ -> pure (Just [audioEvent])}}) $ \app -> do
        budget <- Pipeline.newBudget app
        _ <- Pipeline.ingestWindow app budget prof lo hi
        obss <- Db.observationsBetween (appDb app) t0 (addSecs 100000 t0)
        map perceptionKind obss @?= ["sound"]
  , testCase "a failed fetch (Nothing) holds the watermark at lo" $
      withFakeApp (\a -> a {appFrigate = fakeFrigate {Frigate.recentEvents = \_ _ _ _ -> pure Nothing}}) $ \app -> do
        budget <- Pipeline.newBudget app
        wm <- Pipeline.ingestWindowSafe app budget prof lo hi
        wm @?= lo
  , testCase "a completed NoMedia event is stepped past; an in-progress one holds below it" $ do
      -- Completed (end set) with no media: handled, so the watermark advances to hi.
      withFakeApp (\a -> a {appFrigate = fakeFrigate {Frigate.recentEvents = \_ _ _ _ -> pure (Just [noMediaEvent {feEnd = Just (feStart noMediaEvent + 5)}])}}) $ \app -> do
        budget <- Pipeline.newBudget app
        wm <- Pipeline.ingestWindow app budget prof lo hi
        wm @?= hi
      -- In progress (end Nothing): media may still arrive, so hold the watermark at
      -- or below the event's start. The hold only lasts while the event is inside the
      -- retry window (a persistently unreadable one is later abandoned), so anchor it
      -- just before the real clock's now rather than the fixed past t0.
      withFakeApp id $ \app0 -> do
        nowP <- posixSecs <$> Clock.now (appClock app0)
        let recent = noMediaEvent {feEnd = Nothing, feStart = nowP - 60}
            app = app0 {appFrigate = fakeFrigate {Frigate.recentEvents = \_ _ _ _ -> pure (Just [recent])}}
        budget <- Pipeline.newBudget app
        wm <- Pipeline.ingestWindow app budget prof (nowP - 120) (nowP + 120)
        assertBool "watermark held below the in-progress event" (wm <= feStart recent)
  , testCase "a snapshot-only event stores a photo observation via the vision path" $
      withFakeApp
        ( \a ->
            a
              { appFrigate =
                  fakeFrigate
                    { Frigate.recentEvents = \_ _ _ _ -> pure (Just [snapEvent])
                    , Frigate.eventSnapshot = \_ -> pure (Just "jpeg-bytes")
                    }
              , appLlm = fakeLlm (const (pure (answer sceneReply)))
              }
        )
        $ \app -> do
          budget <- Pipeline.newBudget app
          _ <- Pipeline.ingestWindow app budget prof lo hi
          obss <- Db.observationsBetween (appDb app) t0 (addSecs 100000 t0)
          map perceptionKind obss @?= ["scene"]
  , testCase "the cooldown carries across pages: a same-key burst split over two passes skips the second" $
      -- Two audio events on the same (camera, label) 300s apart, under the 600s cooldown,
      -- ingested in two separate ingestWindow passes over one database. The second pass
      -- seeds its cooldown map from the first event, already stored just below its lo, so
      -- the second event is cooldown-skipped. An in-memory-only map, empty each pass,
      -- would let it through.
      withFakeApp id $ \app0 -> do
        let evA = audioEvent {feId = "burst-a", feStart = posixSecs t0}
            evB = audioEvent {feId = "burst-b", feStart = posixSecs t0 + 300}
            withEvents evs = app0 {appFrigate = fakeFrigate {Frigate.recentEvents = \_ _ _ _ -> pure (Just evs)}}
            countStored = length <$> Db.observationsBetween (appDb app0) t0 (addSecs 100000 t0)
        budget <- Pipeline.newBudget app0
        -- Pass 1: window straddling only evA; it stores.
        _ <- Pipeline.ingestWindow (withEvents [evA]) budget prof (posixSecs t0 - 1) (posixSecs t0 + 1)
        afterFirst <- countStored
        -- Pass 2: window over evB, whose lo sits just after evA so the seed lookback
        -- [lo - 600, lo) covers evA; evB is 300s later, inside the cooldown, so skipped.
        _ <- Pipeline.ingestWindow (withEvents [evB]) budget prof (posixSecs t0 + 1) (posixSecs t0 + 400)
        afterSecond <- countStored
        (afterFirst, afterSecond) @?= (1, 1)
  ]
  where
    prof = catProfile
    lo = posixSecs t0
    hi = posixSecs (addSecs 100000 t0)

-- A codec-valid Scene reply built by round-tripping a real Scene value through its
-- own JSON codec, so the vision decoder accepts it without any hand-written JSON.
sceneReply :: Text
sceneReply =
  TE.decodeUtf8 . LBS.toStrict . encode $
    emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]}

-- --------------------------------------------------------------------------- --
-- (e) Web handlers
-- --------------------------------------------------------------------------- --

-- Servant handlers run via 'runHandler', which surfaces the ServerError a rejected
-- request throws; pin the status codes and the transcription-off message.
webUnits :: [TestTree]
webUnits =
  [ testCase "dayH rejects an unrecognised day with a 400" $
      withFakeApp id $ \app -> do
        r <- runHandler (Web.dayH app "not-a-day")
        case r of
          Left ServerError {errHTTPCode = 400} -> pure ()
          other -> assertFailure ("expected a 400, got " <> show (void other))
  , testCase "observationH 404s on an unknown id" $
      withFakeApp id $ \app -> do
        r <- runHandler (Web.observationH app 999)
        case r of
          Left ServerError {errHTTPCode = 404} -> pure ()
          other -> assertFailure ("expected a 404, got " <> show (void other))
  , testCase "transcribeH reports transcription-off when the flag is False" $
      withFakeApp (\a -> a {appFrigate = fakeFrigate {Frigate.transcriptionEnabled = pure False}}) $ \app -> do
        r <- runHandler (Web.transcribeH app 1)
        case r of
          Left ServerError {errHTTPCode = 400, errBody = b} ->
            assertBool "the body names the disabled feature" ("turned off" `T.isInfixOf` TE.decodeUtf8 (LBS.toStrict b))
          other -> assertFailure ("expected a 400 with the disabled message, got " <> show (void other))
  , testCase "keeping an event owns its media, so it outlives Frigate; un-keeping frees it" $
      -- The heart of the ownership model: a kept moment's still + clip are copied out
      -- of Frigate, so the event-media routes keep serving them after Frigate prunes.
      withFakeApp
        (\a -> a {appFrigate = fakeFrigate {Frigate.eventSnapshot = \_ -> pure (Just "snap"), Frigate.eventClip = \_ -> pure (Just "clip")}})
        $ \app -> do
          now <- Clock.now (appClock app)
          let sc = emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) Sleeping noBehaviors Nothing]}
              ev = NewObservation now (Camera "office") (FromEvent (FrigateMeta (EventId "ev-keep") "cat" 0.9 True True)) (Seen sc)
          Db.insertObservation (appDb app) ev
          o : _ <- Db.observationsBetween (appDb app) (addSecs (-60) now) (addSecs 60 now)
          let ObsId oid = obsId o
          _ <- runHandler (Web.keepsakeAddH app oid (Web.KeepsakeReq Nothing Nothing))
          -- Frigate now has nothing; the owned copies must still serve.
          let gone = app {appFrigate = fakeFrigate {Frigate.eventSnapshot = \_ -> pure Nothing, Frigate.eventClip = \_ -> pure Nothing}}
          rs <- runHandler (Web.eventSnapH gone "ev-keep")
          rc <- runHandler (Web.eventClipH gone "ev-keep")
          (getResponse <$> rs) @?= Right "snap"
          (getResponse <$> rc) @?= Right "clip"
          -- Un-keeping frees the owned copy: borrowed again, and Frigate has none, so 404.
          k : _ <- Db.listKeepsakes (appDb app) Nothing
          _ <- runHandler (Web.keepsakeDelH app (Db.kId k))
          rs2 <- runHandler (Web.eventSnapH gone "ev-keep")
          either (const True) (const False) rs2 @?= True
  , testCase "momentsH browses, enriches, and a facet filters the total" $
      withFakeApp id $ \app -> do
        now <- Clock.now (appClock app)
        let mk act off =
              NewObservation
                (addSecs off now)
                (Camera "office")
                PeriodicSample
                (Seen emptyScene {appearances = [Appearance (AnAnimal (Species "cat")) act noBehaviors Nothing]})
        Db.insertObservation (appDb app) (mk Sleeping 10)
        Db.insertObservation (appDb app) (mk Playing 20)
        let browse act =
              Web.momentsH app Nothing Nothing Nothing act Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 50)
        rAll <- runHandler (browse Nothing)
        (totalOf <$> rAll) @?= Right (Just 2)
        rPlay <- runHandler (browse (Just "playing"))
        (totalOf <$> rPlay) @?= Right (Just 1)
  , testCase "pet add/edit/archive round-trips through the profile (409 on a dup id)" $
      withFakeApp id $ \app -> do
        let pet = Pet (PetId "yuki") "Yuki" (Species "cat") "grey cat" Nothing Nothing Nothing
        _ <- runHandler (Web.petAddH app (Web.AddPetReq pet Nothing))
        p1 <- Db.getProfile (appDb app)
        map petId (pets p1) @?= [PetId "yuki"]
        dup <- runHandler (Web.petAddH app (Web.AddPetReq pet Nothing))
        case dup of
          Left ServerError {errHTTPCode = 409} -> pure ()
          other -> assertFailure ("expected a 409, got " <> show (void other))
        _ <- runHandler (Web.petEditH app "yuki" (Web.EditPetReq (Just "Yuki II") Nothing Nothing Nothing Nothing False))
        p2 <- Db.getProfile (appDb app)
        map petName (pets p2) @?= ["Yuki II"]
        _ <- runHandler (Web.petArchiveH app "yuki")
        p3 <- Db.getProfile (appDb app)
        any ((/= Nothing) . petArchivedAt) (pets p3) @?= True
  , testCase "a pet photo stores and serves on add, and clears on a remove edit" $
      withFakeApp id $ \app -> do
        let pet = Pet (PetId "rex") "Rex" (Species "dog") "big dog" Nothing Nothing Nothing
        -- "aGVsbG8=" is base64 for "hello"; storePetPhoto keeps whatever decodes, so a
        -- real JPEG is not needed to exercise store/serve.
        _ <- runHandler (Web.petAddH app (Web.AddPetReq pet (Just "aGVsbG8=")))
        p1 <- Db.getProfile (appDb app)
        assertBool "a content token is recorded on the pet" (map petPhoto (pets p1) /= [Nothing])
        rphoto <- runHandler (Web.petPhotoH app "rex")
        (getResponse <$> rphoto) @?= Right "hello"
        -- A remove edit drops the token and the photo route 404s.
        _ <- runHandler (Web.petEditH app "rex" (Web.EditPetReq Nothing Nothing Nothing Nothing Nothing True))
        p2 <- Db.getProfile (appDb app)
        map petPhoto (pets p2) @?= [Nothing]
        r404 <- runHandler (Web.petPhotoH app "rex")
        either (const True) (const False) r404 @?= True
  ]

-- --------------------------------------------------------------------------- --
-- (f) API contract: handler-produced shapes
-- --------------------------------------------------------------------------- --

-- The JSON contract for response shapes whose keys are only pinnable by running
-- the handler (hand-built `object [...]` in Web.hs, so there is no type to
-- encode directly). Driven through `withFakeApp`/`runHandler`, empty DB, fake
-- Frigate, to keep things fast and deterministic.
-- Keys sourced from frontend/src/lib/api.ts.
contractHandlerUnits :: [TestTree]
contractHandlerUnits =
  [ testCase "overviewH JSON keys match the client contract (Overview)" $
      withFakeApp id $ \app -> do
        r <- runHandler (Web.overviewH app)
        case r of
          Left e  -> assertFailure ("overviewH failed: " <> show e)
          Right v -> jsonKeysV (encode v) @?= sort ["cameras", "pendingReview"]
  , testCase "batchH JSON keys match the client contract (BatchStatus)" $
      withFakeApp id $ \app -> do
        r <- runHandler (Web.batchH app Nothing)
        case r of
          Left e  -> assertFailure ("batchH failed: " <> show e)
          Right v -> jsonKeysV (encode v) @?= sort ["running", "last"]
  , testCase "statusH JSON keys match the client contract (Status)" $
      -- Point the EffSettings at a refused port so the two probes fail instantly
      -- rather than waiting for the 5s timeout in Probe.reachable.
      withFakeApp id $ \app -> do
        atomically (writeTVar (appSettings app) (EffSettings {efFrigate = BaseUrl "http://127.0.0.1:1", efLlm = BaseUrl "http://127.0.0.1:1", efModel = ModelName "m", efTz = "UTC"}))
        r <- runHandler (Web.statusH app)
        case r of
          Left e  -> assertFailure ("statusH failed: " <> show e)
          Right v -> jsonKeysV (encode v) @?= sort ["frigate", "model", "cameras"]
  ]

-- | The sorted top-level object keys of an encoded JSON value ([] if not an
-- object), mirroring `jsonKeys` in Main.hs for the handler-produced Value tests.
jsonKeysV :: LBS.ByteString -> [Text]
jsonKeysV bs = case decode bs of
  Just (Object o) -> sort (map Key.toText (KeyMap.keys o))
  _               -> []

-- | The @total@ field of a browse-page Value, if present and numeric.
totalOf :: Value -> Maybe Int
totalOf v = case KeyMap.lookup "total" (objOf v) of
  Just (Number n) -> Just (round n)
  _               -> Nothing

-- --------------------------------------------------------------------------- --
-- (g) Config
-- --------------------------------------------------------------------------- --

-- The one listen-address parser both the validator and the warp runner share.
configUnits :: [TestTree]
configUnits =
  [ testCase "parseListen round-trips a host:port and rejects the rest" $ do
      fmap (fmap portNumber) (parseListen "127.0.0.1:8116") @?= Just ("127.0.0.1", 8116)
      fmap (fmap portNumber) (parseListen "[::1]:8116") @?= Just ("::1", 8116)
      parseListen "garbage" @?= Nothing
      parseListen ":8116" @?= Nothing
      parseListen "host:0" @?= Nothing
  ]

-- --------------------------------------------------------------------------- --
-- Fakes + fixtures
-- --------------------------------------------------------------------------- --

-- | A Frigate handle where every field is a benign stub (empty list / Nothing);
-- each test overrides only the field it drives.
fakeFrigate :: Frigate.Handle
fakeFrigate =
  Frigate.Handle
    { Frigate.onlineCameras = \_ -> pure []
    , Frigate.listCameras = pure []
    , Frigate.latestFrame = \_ -> pure Nothing
    , Frigate.recentEvents = \_ _ _ _ -> pure (Just [])
    , Frigate.eventSnapshot = \_ -> pure Nothing
    , Frigate.eventClip = \_ -> pure Nothing
    , Frigate.transcribe = \_ -> pure (Left Frigate.NoTranscript)
    , Frigate.transcriptionEnabled = pure True
    , Frigate.mediaRetention = pure mempty
    }

-- | An Llm handle backed by a scripted @chat@.
fakeLlm :: (ChatRequest -> IO AssistantMessage) -> Llm.Handle
fakeLlm f = Llm.Handle {Llm.chat = f}

-- | A model turn that is just an answer (no tool calls).
answer :: Text -> AssistantMessage
answer t = AssistantMessage {amContent = Just t, amReasoning = Nothing, amToolCalls = []}

-- | A model turn that requests one tool with empty arguments.
toolTurn :: Text -> AssistantMessage
toolTurn name =
  AssistantMessage
    { amContent = Nothing
    , amReasoning = Nothing
    , amToolCalls = [ToolCall {tcId = "call_1", tcName = name, tcArgs = "{}"}]
    }

-- | The role string of a chat message, so the scripted model can tell a tool
-- result turn (role "tool") from the opening user turn.
roleOf :: Llm.ChatMessage -> Text
roleOf m = case Llm.msgRole m of
  Llm.System    -> "system"
  Llm.User      -> "user"
  Llm.Assistant -> "assistant"
  Llm.ToolRole  -> "tool"

-- | Build a minimal 'App' over a real temp DB, a UTC clock, real (side-effect-free)
-- Ffmpeg and a no-op Ntfy, and the two fakes; @tweak@ overrides the Frigate/Llm
-- handles per test. Config paths point into the temp dir.
withFakeApp :: (App -> App) -> (App -> IO a) -> IO a
withFakeApp tweak k =
  withSystemTempDirectory "petreport-eff" $ \dir ->
    Db.withHandle (dbTracer renderingTracer) (dir </> "eff.db") $ \db ->
      Clock.withHandle (pure "UTC") $ \clock ->
        Ffmpeg.withHandle $ \ffmpeg ->
          Ntfy.withHandle (ntfyTracer renderingTracer) "http://localhost/none" Nothing $ \ntfy -> do
            urls <- newTVarIO (EffSettings {efFrigate = BaseUrl "http://f", efLlm = BaseUrl "http://l", efModel = ModelName "m", efTz = "UTC"})
            jobs <- newJobs
            retention <- newTVarIO mempty
            batchLock <- newMVar ()
            let cfg = fakeConfig dir
            k $
              tweak
                App
                  { appConfig = cfg
                  , appBaseConfig = cfg
                  , appClock = clock
                  , appDb = db
                  , appLlm = fakeLlm (const (pure (answer "")))
                  , appFrigate = fakeFrigate
                  , appFfmpeg = ffmpeg
                  , appNtfy = ntfy
                  , appTracer = renderingTracer
                  , appRuntime =
                      Runtime
                        { rtSettings = urls
                        , rtJobs = jobs
                        , rtRetention = retention
                        , rtBatchLock = batchLock
                        }
                  }

-- | A plain 'Config' record with temp-dir paths and the ingest label vocabulary
-- the pipeline reads (dog/cat visual, speech audio).
fakeConfig :: FilePath -> Config
fakeConfig dir =
  Config
    { cfgDbPath = dir </> "eff.db"
    , cfgQueueDir = dir </> "queue"
    , cfgProofDir = dir </> "proof"
    , cfgListen = "127.0.0.1:8116"
    , cfgFrigateUrl = "http://localhost:8114"
    , cfgLlamaUrl = "http://localhost:8080"
    , cfgNtfyUrl = "http://localhost:8106/pet-report"
    , cfgVisionModel = "test-model"
    , cfgPublicUrl = Nothing
    , cfgTimeZone = "UTC"
    , cfgCameras = [Camera "office"]
    , cfgPetLabels = ["dog", "cat"]
    , cfgAudioLabels = ["speech", "bark", "meow"]
    , cfgRetentionPollSecs = 900
    , cfgMediaDir = dir </> "media"
    , cfgCaptureSecs = 600
    , cfgBatchHours = mapMaybe mkHour [8, 20]
    }

-- | A profile with one cat, so a lone-cat sighting resolves unambiguously.
catProfile :: Profile
catProfile =
  emptyProfile
    { pets = [Pet (PetId "dexter") "Dexter" (Species "cat") "orange cat" Nothing Nothing Nothing]
    , cameras = [CameraRoom "office" "Office" True]
    }

-- A base Frigate event; the specific tests tweak the flags/labels they need.
mkEvent :: FrigateEvent
mkEvent =
  FrigateEvent
    { feId = "ev-base"
    , feCamera = "office"
    , feLabel = "cat"
    , feScore = 0.9
    , feStart = posixSecs t0
    , feEnd = Just (posixSecs t0 + 5)
    , feFalsePositive = False
    , feHasClip = False
    , feHasSnapshot = False
    }

audioEvent :: FrigateEvent
audioEvent = mkEvent {feId = "ev-audio", feLabel = "speech", feHasClip = True, feHasSnapshot = True}

-- No clip, no snapshot: exercises the NoMedia branch.
noMediaEvent :: FrigateEvent
noMediaEvent = mkEvent {feId = "ev-nomedia", feLabel = "cat", feHasClip = False, feHasSnapshot = False}

-- A snapshot-only event: exercises the vision path (snapshot fetched, then analysed).
snapEvent :: FrigateEvent
snapEvent = mkEvent {feId = "ev-snap", feLabel = "cat", feHasClip = False, feHasSnapshot = True}

-- --------------------------------------------------------------------------- --
-- Small helpers
-- --------------------------------------------------------------------------- --

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 7 8) 0

addSecs :: Double -> UTCTime -> UTCTime
addSecs s (UTCTime d dt) = UTCTime d (dt + realToFrac s)

posixSecs :: UTCTime -> Double
posixSecs = realToFrac . utcTimeToPOSIXSeconds

-- | The object behind a 'Value' known to be a JSON object (the request shape
-- 'requestJson' always builds), so a test can assert on its keys.
objOf :: Value -> KeyMap.KeyMap Value
objOf (Object o) = o
objOf _          = error "requestJson did not produce a JSON object"

-- | Whether an encoded request object carries a given key.
hasKey :: Key.Key -> KeyMap.KeyMap Value -> Bool
hasKey = KeyMap.member

-- | The @kind@ tag of an observation's perception ("sound" / "scene"), for
-- asserting what ingest stored without unpacking the whole blob.
perceptionKind :: Observation -> Text
perceptionKind o = case perception o of
  Seen _  -> "scene"
  Heard _ -> "sound"

-- | Read and decode a fixture by relative path (the suite runs from the package
-- root, so @test/fixtures/...@ resolves).
decodeFixture :: (FromJSON a) => FilePath -> IO a
decodeFixture name = do
  m <- decodeFileStrict ("test/fixtures" </> name)
  maybe (assertFailure ("could not decode fixture " <> name)) pure m

-- | Run an IO action, capturing any exception (for the agent fault-propagation test).
tryAny :: IO a -> IO (Either SomeException a)
tryAny = try
