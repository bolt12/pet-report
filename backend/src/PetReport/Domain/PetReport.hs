-- | Per-pet analytics for the Pets screen, assembled from the pure 'presence' and 'trends'
-- aggregations plus hour-of-day and room histograms. Only the deterministic parts are
-- computed here; the model-written recap and wellbeing text, and the saved keepsake, are
-- left 'Nothing' for the web layer to fill from the database.
--
-- Rest and balance figures are proportions of sighting counts, not measured durations, and
-- are labelled "% of sightings" so a sampling artifact never reads to an owner as measured
-- time.
module PetReport.Domain.PetReport
  ( PetInsights (..)
  , Tile (..)
  , Habit (..)
  , Spot (..)
  , BalanceV (..)
  , WellbeingV (..)
  , WellbeingKind (..)
  , wellbeingKindText
  , wellbeingKindFromText
  , LastSeenV (..)
  , RecapV (..)
  , StatPair (..)
  , KeepsakeV (..)
  , insightsFor
  , insightsForAll
  , hourHistogram
  , roomDistribution
  , petStatFor
  , petStatOver
  , monthTrends
  , monthStatFor
  , monthStatPairs
  , anomalyChip
  ) where

import           Data.Aeson          (ToJSON (..), genericToJSON)
import           Data.Int            (Int64)
import           Data.List           (sortOn)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Data.Maybe          (fromMaybe, isNothing, listToMaybe)
import           Data.Ord            (Down (..))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.Time.LocalTime (localTimeOfDay, todHour)
import           Data.Time.Zones     (TZ, utcToLocalTimeTZ)
import           GHC.Generics        (Generic)

import           PetReport.Domain.Observation (Observation (..))
import           PetReport.Domain.Perception  (Appearance (..))
import           PetReport.Domain.Profile     (CameraRoom, Overrides, Pet (..),
                                               Roster, roomOf,
                                               uniquePetOfSpecies)
import           PetReport.Domain.Stats       (resolvedStatsMap, StoredStats (..), PetStat (..), SubjectKey (..),
                                               emptyPetStat, statOf,
                                               subjectAppearances)
import           PetReport.Domain.Trends      (DayTrend (..), trends)
import           PetReport.Domain.Types       (ObsId (..), PetId, activityText,
                                               cameraText, petIdText,
                                               speciesText)
import           PetReport.Domain.View        (Chip (..), ChipKind (..))
import           PetReport.Util               (capitalize, prefixed, tshow)

-- --------------------------------------------------------------------------- --
-- View types
-- --------------------------------------------------------------------------- --

data Tile = Tile
  { tlLabel :: Text
  , tlValue :: Text
  -- ^ Semantic token: "yes" | "no" | "lots" | "some" | "none".
  , tlSub   :: Text
  , tlKind  :: Text
  -- ^ "good" | "plain" | "watch".
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Tile where
  toJSON = genericToJSON (prefixed 2)

-- | A weekly habit row: per-day counts ('hbValues', oldest first), the rounded week
-- mean as 'hbUsual', and whether today fell below a nonzero usual.
data Habit = Habit
  { hbLabel      :: Text
  , hbSummary    :: Text
  , hbUsual      :: Int
  , hbValues     :: [Int]
  , hbBelowUsual :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Habit where
  toJSON = genericToJSON (prefixed 2)

-- | One of a pet's favourite places: the room's display label, the cameras that label
-- resolved from, and the share of the pet's sightings that landed there.
--
-- The cameras are carried because 'spRoom' is a DISPLAY label and cannot be used as a
-- query key: 'PetReport.Domain.Profile.roomOf' falls back to a title-cased camera id when a
-- camera is missing from the profile or its room was saved blank, and no saved room label
-- ever equals that. A link built from the label alone therefore filtered on a room nobody
-- had, and opened nothing. Linking on the camera ids the spot actually counted makes the
-- number and the list agree whatever the label says.
data Spot = Spot
  { spRoom    :: Text
  , spCameras :: [Text]
  , spPct     :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Spot where
  toJSON = genericToJSON (prefixed 2)

data BalanceV = BalanceV
  { blRestPct :: Int
  , blCaption :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BalanceV where
  toJSON = genericToJSON (prefixed 2)

-- | The deterministic weekly wellbeing verdict: 'Settled' when nothing needs attention,
-- 'Flagged' when there is one gentle thing to look at. The wire form is "good" and "watch"
-- (see 'wellbeingKindText'), which is what the frontend and the @pet_summaries@ cache
-- speak. The constructor names avoid clashing with 'ChipKind''s Good and Watch.
data WellbeingKind = Settled | Flagged
  deriving stock (Eq, Show, Bounded, Enum)

-- | Why a pet is flagged. The labels name the reason rather than saying something
-- task-shaped, because neither reason is a task: nothing an owner can click clears either
-- one. Reviewing a moment sets its @reviewed@ flag, which drives the separate "moments to
-- review" count and never touches this verdict. Both clear on their own when the data moves
-- on, so a label that reads like a chore just sends the owner looking for a button that is
-- not there.
data WatchReason
  = ConcernNoted
  -- ^ An observation this week carried a concern, or a suspected accident or injury. Clears
  -- when that observation ages out of the seven-day window.
  | NoMealSeenToday
  -- ^ Seen today, never at the bowl today, on a week where meals were seen on other days.
  -- Clears the moment a meal is seen. "Seen" is the operative word: the cameras missing a
  -- meal looks exactly like a skipped one, which is why the label says seen.
  deriving stock (Eq, Show)

-- | The owner-facing label for a reason, short enough for a chip.
watchText :: WatchReason -> Text
watchText r = case r of
  ConcernNoted    -> "a concern this week"
  NoMealSeenToday -> "no meals seen today"

-- | The wire/DB string for a verdict. This is the closed vocabulary the frontend
-- and the summaries cache depend on; keep it byte-identical.
wellbeingKindText :: WellbeingKind -> Text
wellbeingKindText k = case k of
  Settled -> "good"
  Flagged -> "watch"

-- | Parse a stored verdict. Tolerant on purpose: only "watch" reads as 'Flagged', and
-- anything else falls back to the neutral 'Settled'. @pet_summaries@ is a regenerable
-- cache, so a garbage kind must never crash a read; the worst case is one neutral verdict
-- until the next batch.
wellbeingKindFromText :: Text -> WellbeingKind
wellbeingKindFromText t = case t of
  "watch" -> Flagged
  _       -> Settled

instance ToJSON WellbeingKind where
  -- Emit the "good"/"watch" string, never the bare constructor name.
  toJSON = toJSON . wellbeingKindText

data WellbeingV = WellbeingV
  { wbKind :: WellbeingKind
  -- ^ Serialises to "good" | "watch".
  , wbText :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON WellbeingV where
  toJSON = genericToJSON (prefixed 2)

data LastSeenV = LastSeenV
  { lsObsId :: Int64
  , lsAt    :: UTCTime
  , lsRoom  :: Text
  , lsLine  :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LastSeenV where
  toJSON = genericToJSON (prefixed 2)

data StatPair = StatPair
  { stK :: Text
  , stV :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON StatPair where
  toJSON = genericToJSON (prefixed 2)

data RecapV = RecapV
  { rcRange :: Text
  , rcLine  :: Text
  , rcStats :: [StatPair]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RecapV where
  toJSON = genericToJSON (prefixed 2)

data KeepsakeV = KeepsakeV
  { kpId      :: Int64
  , kpObsId   :: Int64
  , kpCaption :: Maybe Text
  , kpRoom    :: Text
  , kpImg     :: Maybe Text
  , kpMedia   :: Text
  , kpAt      :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON KeepsakeV where
  toJSON = genericToJSON (prefixed 2)

-- | Everything the Pets screen renders for one pet. 'piSpark' is per-day sightings over the
-- week, oldest first; 'piRhythm' the 24 local-hour buckets; 'piRhythmMarks' the hours a meal
-- was seen. The DB-backed fields (recap, wellbeing text, keepsake) and the month fields
-- start blank here for the web layer to fill.
data PetInsights = PetInsights
  { piId          :: Text
  , piName        :: Text
  , piSpecies     :: Text
  , piDescription :: Text
  , piCaveat      :: Maybe Text
  , piSeen        :: Int
  , piGlance      :: [Chip]
  , piNote        :: Chip
  , piTiles       :: [Tile]
  , piSpark       :: [Int]
  , piHabits      :: [Habit]
  , piRhythm      :: [Int]
  , piRhythmMarks :: [Int]
  , piBalance     :: BalanceV
  , piSpots       :: [Spot]
  , piWellbeing   :: WellbeingV
  , piLastSeen    :: Maybe LastSeenV
  , piRecap       :: Maybe RecapV
  , piKeepsake    :: Maybe KeepsakeV
  , piMonthSeen   :: Int
  , piMonthStats  :: [StatPair]
  , piAnomaly     :: Maybe Chip
  }
  deriving stock (Generic)

instance ToJSON PetInsights where
  toJSON = genericToJSON (prefixed 2)

-- --------------------------------------------------------------------------- --
-- Assembly
-- --------------------------------------------------------------------------- --

-- | Deterministic insights for a set of pets over one week window. The per-day 'trends' pass
-- aggregates ALL subjects, so it runs ONCE here and each pet reads its own @KPet@ bucket out
-- of the shared result. One pass instead of one per pet, on the hot path behind @/api/pets@.
--
-- @roster@ is the full roster, used for identity resolution; @tiles@ is the subset to
-- render, usually the active pets.
insightsForAll :: TZ -> Overrides -> [CameraRoom] -> Roster -> UTCTime -> [Observation] -> [Pet] -> [(Pet, PetInsights)]
insightsForAll tz ov crs roster now weekObs tiles =
  let ds = trends tz ov roster 7 now weekObs
   in [(pet, insightsFrom tz ov crs roster ds pet weekObs) | pet <- tiles]

-- | One pet's deterministic insights over the week's observations. The window ends at "now"
-- and must include today. Recap, wellbeing text and keepsake are left blank for the caller
-- to fill from the database. Computes the week 'trends' for this one pet; 'insightsForAll'
-- shares that pass across the roster.
insightsFor :: TZ -> Overrides -> [CameraRoom] -> Roster -> UTCTime -> Pet -> [Observation] -> PetInsights
insightsFor tz ov crs roster now pet weekObs =
  insightsFrom tz ov crs roster (trends tz ov roster 7 now weekObs) pet weekObs

-- | Assemble one pet's insights from a PRECOMPUTED week 'trends' result. The caller owns
-- the shared @[DayTrend]@, and this reads only the pet's @KPet@ bucket out of each day, so
-- many pets share one trends pass without changing what any single pet sees.
insightsFrom :: TZ -> Overrides -> [CameraRoom] -> Roster -> [DayTrend] -> Pet -> [Observation] -> PetInsights
insightsFrom tz ov crs roster ds pet weekObs =
  let key = KPet (petId pet)
      weekStats = [Map.findWithDefault emptyPetStat key (resolvedStatsMap (dtStats dt)) | dt <- ds]
      todayStat = if null weekStats then emptyPetStat else last weekStats
      weekTotal = foldl' (<>) emptyPetStat weekStats
      seen = psSightings todayStat
      restLots = restedALot todayStat
      spark = map psSightings weekStats
      -- The reason is the single source of truth and the verdict is derived from it, so the
      -- two cannot drift apart and claim a pet is flagged for nothing (or the reverse).
      -- Baseline for meals is "ate on some earlier day this week". A rounded mean would
      -- collapse a real 3-days-in-7 habit (mean 0.43) to 0 and never flag a skipped meal.
      mwatch
        | psConcerns weekTotal > 0 = Just ConcernNoted
        | seen > 0 && psAte todayStat == 0 && sum (map psAte weekStats) > 0 = Just NoMealSeenToday
        | otherwise = Nothing
      kind = maybe Settled (const Flagged) mwatch
      rmDist = roomDistribution crs ov roster (petId pet) weekObs
      total = max 1 (sum [n | (_, _, n) <- rmDist])
   in PetInsights
        { piId = petIdText (petId pet)
        , piName = petName pet
        , piSpecies = speciesText (petSpecies pet)
        , piDescription = petDescription pet
        , piCaveat = petNotes pet
        , piSeen = seen
        , piGlance = glanceChips seen todayStat restLots mwatch
        , piNote =
            if seen == 0
              then Chip "Not seen yet" Info
              else maybe (Chip "Nothing of concern" Good) (\r -> Chip (watchText r) Watch) mwatch
        , piTiles = tilesFor todayStat
        , piSpark = spark
        , piHabits = habitsFor weekStats
        , piRhythm = hourHistogram tz ov key roster weekObs
        , piRhythmMarks = mealMarks tz ov key roster weekObs
        , piBalance = balanceFor weekTotal
        , piSpots = [Spot r cams (pct c total) | (r, cams, c) <- take 4 rmDist]
        , piWellbeing = WellbeingV kind Nothing
        , piLastSeen = lastSeenFor tz ov crs roster key weekObs
        , piRecap = Nothing
        , piKeepsake = Nothing
        , piMonthSeen = 0
        , piMonthStats = []
        , piAnomaly = Nothing
        }

-- | Whether a pet rested through at least half its sightings this period: the "rested a lot"
-- verdict behind the glance chip and the Rest tile. Not the same as the balance percentage
-- in 'BalanceV', which answers "of its rest plus active time, how much was rest".
restedALot :: PetStat -> Bool
restedALot st = psRest st * 2 >= max 1 (psSightings st)

glanceChips :: Int -> PetStat -> Bool -> Maybe WatchReason -> [Chip]
glanceChips 0 _ _ _ = []
glanceChips seen st restLots mwatch =
  Chip ("seen " <> tshow seen <> "\215") Info
    : concat
      [ [Chip "ate" Good | psAte st > 0]
      , [Chip "drank" Good | psDrank st > 0]
      , [Chip "rested a lot" Good | restLots && isNothing mwatch]
      , maybe [] (\r -> [Chip (watchText r) Watch]) mwatch
      ]

tilesFor :: PetStat -> [Tile]
tilesFor st =
  [ countTile "Meals" (psAte st) "at the bowl"
  , countTile "Water" (psDrank st) "drinking"
  , restTile
  , countTile "Litter" (psEliminated st) "the tray"
  , countTile "Play" (psPlayed st) "playing"
  ]
  where
    countTile lbl n noun =
      Tile
        lbl
        (if n > 0 then "yes" else "no")
        (if n > 0 then "seen " <> noun <> " " <> tshow n <> "\215" else "not seen " <> noun)
        (if n > 0 then "good" else "plain")
    restTile =
      let r = psRest st
          val
            | r == 0 = "none"
            | restedALot st = "lots"
            | otherwise = "some"
       in Tile "Rest" val (tshow r <> " restful sightings") (if r > 0 then "good" else "plain")

habitsFor :: [PetStat] -> [Habit]
habitsFor weekStats = map mk defs
  where
    defs =
      [ ("Meals", psAte)
      , ("Water", psDrank)
      , ("Rest", psRest)
      , ("Litter", psEliminated)
      ]
    mk (lbl, f) =
      let vs = map f weekStats
          usual = mean vs
          today = if null vs then 0 else last vs
       in Habit lbl (rangeText vs) usual vs (today < usual && usual > 0)

balanceFor :: PetStat -> BalanceV
balanceFor st =
  let denom = max 1 (psRest st + psActive st)
      restPct = round (100 * fromIntegral (psRest st) / fromIntegral denom :: Double)
   in BalanceV restPct ("Resting in about " <> tshow restPct <> "% of sightings this week.")

lastSeenFor :: TZ -> Overrides -> [CameraRoom] -> Roster -> SubjectKey -> [Observation] -> Maybe LastSeenV
lastSeenFor _tz ov crs roster key obss =
  listToMaybe
    [ LastSeenV oid (at o) (roomOf crs (camera o)) (lineOf o)
    | o <- sortOn (Down . at) (filter appearsHere obss)
    , let ObsId oid = obsId o
    ]
  where
    appearsHere o = not (null (subjectAppearances ov roster key o))
    lineOf o =
      case map (activityText . activity) (subjectAppearances ov roster key o) of
        (a : _) -> capitalize a
        []      -> "Seen"

-- --------------------------------------------------------------------------- --
-- Pure histograms (exported for reuse/tests)
-- --------------------------------------------------------------------------- --

-- | Sightings of one subject bucketed by local hour of day (24 buckets).
hourHistogram :: TZ -> Overrides -> SubjectKey -> Roster -> [Observation] -> [Int]
hourHistogram tz ov key roster obss =
  let counts =
        Map.fromListWith
          (+)
          [ (localHour tz (at o), 1 :: Int)
          | o <- obss
          , _ <- subjectAppearances ov roster key o
          ]
   in [Map.findWithDefault 0 h counts | h <- [0 .. 23]]

-- | Hours in which the subject ate or drank (for the meal dots).
mealMarks :: TZ -> Overrides -> SubjectKey -> Roster -> [Observation] -> [Int]
mealMarks tz ov key roster obss =
  dedupSorted
    [ localHour tz (at o)
    | o <- obss
    , ap <- subjectAppearances ov roster key o
    , let s = statOf ap
    , psAte s > 0 || psDrank s > 0
    ]

-- | Room distribution for one pet, most-frequent first, as
-- @(display label, the cameras it covers, sightings)@.
--
-- Grouping is by display label, so two cameras sharing a room name merge into one row, and
-- the cameras that merged are returned alongside. Those ids are the only part of this that
-- a query can filter on; see 'Spot'.
roomDistribution ::
  [CameraRoom] -> Overrides -> Roster -> PetId -> [Observation] -> [(Text, [Text], Int)]
roomDistribution crs ov roster pid obss =
  [ (lbl, Set.toList cams, n)
  | (lbl, (cams, n)) <- sortOn (Down . snd . snd) (Map.toList grouped)
  ]
  where
    grouped =
      Map.fromListWith
        (\(c1, n1) (c2, n2) -> (Set.union c1 c2, n1 + n2))
        [ (roomOf crs (camera o), (Set.singleton (cameraText (camera o)), 1 :: Int))
        | o <- obss
        , _ <- subjectAppearances ov roster (KPet pid) o
        ]

-- --------------------------------------------------------------------------- --
-- Long-window (month) stats, from the facts projection (filled by the web layer)
-- --------------------------------------------------------------------------- --

-- | The month stat for a pet: the sightings the owner explicitly reassigned to it (the
-- @KPet@ bucket from the override join), plus the un-reassigned sightings of its species
-- (the @KSpecies@ bucket) when the roster makes that species unambiguous, matching
-- 'identify'. Two pets of one species therefore each get only their reassigned sightings,
-- while a lone pet of its species is unaffected.
petStatFor :: Roster -> Pet -> StoredStats -> Maybe PetStat
petStatFor roster pet (StoredStats m) =
  let overridden = Map.lookup (KPet (petId pet)) m
      bySpecies = case uniquePetOfSpecies roster (petSpecies pet) of
        Just p | petId p == petId pet -> Map.lookup (KSpecies (petSpecies pet)) m
        _                             -> Nothing
   in case (overridden, bySpecies) of
        (Nothing, Nothing) -> Nothing
        (a, b)             -> Just (fromMaybe emptyPetStat a <> fromMaybe emptyPetStat b)

-- | Sum a pet's stats over an observation window, using the same @KPet@ attribution the week
-- uses, so day <= week <= month holds for nested windows. Computes the window 'trends' for
-- this one pet; 'monthTrends' plus 'monthStatFor' share that pass across the roster.
petStatOver :: TZ -> Overrides -> Roster -> Int -> UTCTime -> Pet -> [Observation] -> PetStat
petStatOver tz ov roster n now pet obss =
  monthStatFor (trends tz ov roster n now obss) pet

-- | The per-day 'trends' over the 30-day month window, computed ONCE per request so every
-- pet's month stat reads the same pass (see 'monthStatFor'). The window size lives here
-- rather than as a bare number at the call site, matching how 'insightsForAll' bakes in its
-- 7 days.
monthTrends :: TZ -> Overrides -> Roster -> UTCTime -> [Observation] -> [DayTrend]
monthTrends tz ov roster = trends tz ov roster 30

-- | Sum a pet's stats out of a PRECOMPUTED window 'trends' result, reading only its @KPet@
-- bucket per day. Sharing the @[DayTrend]@ across pets turns one pass per pet into one pass
-- total. The result matches 'petStatOver', since both fold the same buckets over the same
-- days.
monthStatFor :: [DayTrend] -> Pet -> PetStat
monthStatFor ds pet =
  let key = KPet (petId pet)
   in foldl' (<>) emptyPetStat
        [Map.findWithDefault emptyPetStat key (resolvedStatsMap (dtStats dt)) | dt <- ds]

-- | A compact set of month totals for the Pets screen.
monthStatPairs :: PetStat -> [StatPair]
monthStatPairs st =
  [ StatPair "Seen" (tshow (psSightings st) <> " times")
  , StatPair "Ate" (tshow (psAte st) <> " sightings")
  , StatPair "Litter" (tshow (psEliminated st) <> " sightings")
  , StatPair "Rested" (tshow (pct (psRest st) (max 1 (psSightings st))) <> "% of sightings")
  ]
    -- A count of sightings the model flagged as concerning, not distinct days;
    -- labelled honestly so a sampling artifact is not read as "N bad days".
    ++ [StatPair "Concerns" (tshow (psConcerns st)) | psConcerns st > 0]

-- | A gentle "quieter than usual" flag when today's sightings fall well below the
-- month's daily average (and there is enough baseline to judge).
anomalyChip :: Int -> PetStat -> Maybe Chip
anomalyChip todaySeen st =
  let dailyAvg = psSightings st `div` 30
   in if dailyAvg >= 2 && todaySeen * 2 < dailyAvg
        then Just (Chip "quieter than usual" Watch)
        else Nothing

-- --------------------------------------------------------------------------- --
-- Helpers
-- --------------------------------------------------------------------------- --

localHour :: TZ -> UTCTime -> Int
localHour tz t = todHour (localTimeOfDay (utcToLocalTimeTZ tz t))

pct :: Int -> Int -> Int
pct c total = round (100 * fromIntegral c / fromIntegral total :: Double)

mean :: [Int] -> Int
mean [] = 0
mean xs = round (fromIntegral (sum xs) / fromIntegral (length xs) :: Double)

rangeText :: [Int] -> Text
rangeText [] = "no data"
rangeText vs =
  let lo = minimum vs
      hi = maximum vs
   in if lo == hi
        then tshow lo <> " a day"
        else tshow lo <> " to " <> tshow hi <> " a day"

dedupSorted :: [Int] -> [Int]
dedupSorted = Set.toAscList . Set.fromList
