-- pet-report schema, version 1.
--
-- The greenfield initial schema, applied whole as migration 1 by
-- PetReport.Effect.Db.Migrations. A future schema change adds sql/0002_*.sql and a
-- (2, ...) entry to the migrations list; this file is not edited in place once shipped.
-- Statements are separated by ';'; whole-line '--' comments are stripped by the loader.

-- Observations: a moment's rich perception is the JSON `perception` blob; scalar
-- metadata (time, camera, origin) and the derived facet signals (`confidence`,
-- `uncertain`) are queryable columns.
CREATE TABLE observations (
  id             INTEGER PRIMARY KEY,
  ts             REAL NOT NULL,
  camera         TEXT NOT NULL,
  source         TEXT NOT NULL,
  event_id       TEXT,
  label          TEXT,
  score          REAL,
  perception     TEXT NOT NULL,
  raw_perception TEXT,
  reviewed       INTEGER NOT NULL DEFAULT 0,
  reviewed_at    REAL,
  transcript     TEXT,
  has_clip       INTEGER,
  has_snapshot   INTEGER,
  confidence     REAL,
  uncertain      INTEGER
);

-- At most one observation per Frigate event.
CREATE UNIQUE INDEX observations_event ON observations(event_id) WHERE event_id IS NOT NULL;
-- The (ts, id) keyset order the browse cursor pages over.
CREATE INDEX observations_ts_id ON observations(ts, id);
-- The needs-a-look backlog (contract D1): materialised and partial.
CREATE INDEX observations_needslook ON observations(ts) WHERE uncertain = 1 AND reviewed = 0;

-- The rebuildable per-appearance facts projection (reproject rebuilds it from the blobs).
CREATE TABLE observation_subjects (
  obs_id     INTEGER NOT NULL,
  seq        INTEGER NOT NULL,
  species    TEXT,
  is_person  INTEGER NOT NULL,
  activity   TEXT,
  rest       INTEGER NOT NULL,
  active     INTEGER NOT NULL,
  ate        INTEGER NOT NULL,
  drank      INTEGER NOT NULL,
  slept      INTEGER NOT NULL,
  played     INTEGER NOT NULL,
  groomed    INTEGER NOT NULL,
  eliminated INTEGER NOT NULL,
  accident   INTEGER NOT NULL,
  concern    INTEGER NOT NULL,
  confidence REAL,
  PRIMARY KEY (obs_id, seq),
  FOREIGN KEY (obs_id) REFERENCES observations(id) ON DELETE CASCADE
);
CREATE INDEX obs_subjects_species ON observation_subjects(species);
CREATE INDEX obs_subjects_activity ON observation_subjects(activity);

-- The owner-override source of truth for subject identity (this pet, or visiting).
CREATE TABLE subject_identity (
  obs_id    INTEGER NOT NULL,
  seq       INTEGER NOT NULL,
  pet_id    TEXT,
  visiting  INTEGER NOT NULL DEFAULT 0,
  source    TEXT NOT NULL DEFAULT 'owner',
  confirmed INTEGER NOT NULL DEFAULT 1,
  ts        REAL NOT NULL,
  PRIMARY KEY (obs_id, seq),
  FOREIGN KEY (obs_id) REFERENCES observations(id) ON DELETE CASCADE
);
CREATE INDEX subject_identity_pet ON subject_identity(pet_id);

-- Kept moments (owner keepsakes); their media is owned so it outlives Frigate pruning.
CREATE TABLE keepsakes (
  id      INTEGER PRIMARY KEY,
  obs_id  INTEGER NOT NULL,
  pet_id  TEXT,
  caption TEXT,
  ts      REAL NOT NULL,
  FOREIGN KEY (obs_id) REFERENCES observations(id) ON DELETE CASCADE
);
CREATE UNIQUE INDEX keepsakes_obs ON keepsakes(obs_id);

-- Stored day/period narratives; survive the garbage collection of a day's raw moments.
CREATE TABLE reports (
  id        INTEGER PRIMARY KEY,
  ts        REAL NOT NULL,
  day       TEXT NOT NULL,
  period    TEXT NOT NULL,
  narrative TEXT NOT NULL
);
CREATE UNIQUE INDEX reports_day_period ON reports(day, period);

-- Durable per-day per-pet rollup; survives GC so history is not lost.
CREATE TABLE daily_pet_stats (
  day        TEXT NOT NULL,
  species    TEXT NOT NULL,
  pet_id     TEXT NOT NULL DEFAULT '',
  sightings  INTEGER NOT NULL,
  rest       INTEGER NOT NULL,
  active     INTEGER NOT NULL,
  ate        INTEGER NOT NULL,
  drank      INTEGER NOT NULL,
  slept      INTEGER NOT NULL,
  played     INTEGER NOT NULL,
  groomed    INTEGER NOT NULL,
  eliminated INTEGER NOT NULL,
  concern    INTEGER NOT NULL,
  PRIMARY KEY (day, species, pet_id)
);

-- Cached per-pet weekly summary: the deterministic wellbeing verdict + the LLM recap line.
CREATE TABLE pet_summaries (
  pet_id         TEXT PRIMARY KEY,
  wellbeing_kind TEXT NOT NULL,
  recap_line     TEXT,
  stats          TEXT NOT NULL,
  ts             REAL NOT NULL
);

-- Small key/value stores: pipeline state, and the profile/settings blob.
CREATE TABLE state (k TEXT PRIMARY KEY, v TEXT NOT NULL);
CREATE TABLE settings (k TEXT PRIMARY KEY, v TEXT NOT NULL);
