// Typed client mirroring the backend JSON contract (docs/api-contract.md).
// Field names match the server output exactly.

export interface Pet {
  petId: string
  petName: string
  petSpecies: string
  petDescription: string
  petNotes: string | null
  // When the owner archived (soft-removed) the pet; null = active. Archived pets
  // are hidden and excluded from auto-identification, but their data is kept.
  petArchivedAt: string | null
  // Content token of the pet's avatar photo, or null when it has none. Used to
  // build (and cache-bust) the photo URL; see petPhotoUrl.
  petPhoto: string | null
}

export interface Household {
  catFlap: boolean
  neighbourCat: boolean
  feedTimes: string | null
  notes: string | null
}

export interface CameraRoom {
  camId: string
  room: string
  enabled: boolean
}

export interface ReportPrefs {
  topics: string[]
  freeform: string | null
}

export interface Profile {
  pets: Pet[]
  report: ReportPrefs
  household: Household
  cameras: CameraRoom[]
  frigateUrl: string | null
  modelUrl: string | null
  visionModel: string | null
  timeZone: string | null
  // How many days of raw moments to keep before garbage-collecting the un-kept ones.
  // Honored literally at a one-day minimum, so shortening it actually takes effect.
  // Default 30.
  gcWindowDays: number
  // Seconds between capture passes, overriding PET_REPORT_CAPTURE_SECS when set. Null
  // falls back to that env value.
  captureSecs: number | null
  configuredAt: string | null
}

export interface StatusEndpoint {
  url: string
  reachable: boolean
}
export interface StatusCamera {
  name: string
  room: string
  enabled: boolean
  online: boolean
}
export interface Status {
  frigate: StatusEndpoint
  model: StatusEndpoint
  cameras: StatusCamera[]
}

// A camera Frigate has configured, for setup auto-discovery.
export interface DiscoveredCamera {
  name: string
  online: boolean
}

export type ChipKind = 'good' | 'watch' | 'info'
export interface Chip {
  label: string
  kind: ChipKind
}

export interface SubjectRef {
  petId: string | null
  label: string
  species: string | null
}

export type MediaKind = 'photo' | 'clip' | 'audio' | 'expired'

// A moment's resolved media: its kind and the still/clip URLs (owned copy first,
// with a live Frigate proxy as fallback), served content-addressed by the backend.
export interface Media {
  kind: MediaKind
  stillUrl: string | null
  clipUrl: string | null
}

export interface ObsView {
  id: number
  at: string
  camera: string
  room: string
  subjectLabel: string
  subjects: SubjectRef[]
  activity: string | null
  location: string | null
  chips: Chip[]
  wellbeing: string // normal | concerning | unclear | none
  confidence: number | null
  uncertain: boolean
  description: string | null
  media: Media
  transcript: string | null
  needsReview: boolean
  reviewed: boolean
  kept: boolean
  // When Frigate is expected to prune this moment's borrowed footage (ISO 8601), for
  // a "keep before it is gone" countdown. Null for a kept moment (owned, permanent) or
  // when the retention is unknown.
  clipExpiresAt: string | null
}

// Coarse human presence for a day: whether a person was seen, when last, and how
// many sightings. No identification of who; just "was someone home".
export interface Presence {
  someoneHome: boolean
  personSightings: number
  lastPersonAt: string | null
}

// One pet's (or unattributed species') stats for a day, from the durable rollup or a
// live compute. What keeps an old day meaningful once its raw moments are collected.
export interface DayStat {
  label: string
  petId: string | null
  species: string
  sightings: number
  rest: number
  active: number
  ate: number
  drank: number
  slept: number
  played: number
  groomed: number
  eliminated: number
  concerns: number
}

export interface DayResponse {
  date: string
  moments: ObsView[]
  narrative: string | null
  presence: Presence
  pets: DayStat[]
}

// A page of the faceted moments browse: the items, a cursor to the next page when
// one exists, and the total matching the current facets (first page only).
export interface MomentsPage {
  items: ObsView[]
  nextCursor?: string
  total?: number
}

// The facets of the moments browse, all optional (docs/api-contract.md 3.1).
export interface MomentsQuery {
  from?: string
  to?: string
  pet?: string
  activity?: string
  room?: string
  media?: string
  timeOfDay?: string
  review?: string
  search?: string
  sort?: string
  cursor?: string
  limit?: number
}

// A filter/date preset carried into Review, either from a quick nav ("needs a
// look") or a deep-linked stat card (a date range + facet). `from`/`to` are ISO
// dates that switch Review into range mode; the rest seed the facet filters.
export interface ReviewPreset {
  from?: string
  to?: string
  who?: string[]
  act?: string[]
  room?: string[]
  media?: string[]
  // A coarse time-of-day bucket (morning | afternoon | evening | night).
  timeOfDay?: string
  needs?: boolean
  reviewed?: 'all' | 'reviewed' | 'unreviewed'
}

export interface CameraStatus {
  camera: string
  room: string
  online: boolean
}
export interface Overview {
  cameras: CameraStatus[]
  earliestDay?: string | null
}

export interface Tile {
  label: string
  value: string // yes | no | lots | some | none
  sub: string
  kind: string // good | plain | watch
}
export interface Habit {
  label: string
  summary: string
  usual: number
  values: number[]
  belowUsual: boolean
}
export interface Spot {
  room: string
  pct: number
}
export interface Balance {
  restPct: number
  caption: string
}
export interface Wellbeing {
  kind: string // good | watch
  text: string | null
}
export interface LastSeen {
  obsId: number
  at: string
  room: string
  line: string
}
export interface StatPair {
  k: string
  v: string
}
export interface Recap {
  range: string
  line: string
  stats: StatPair[]
}
// A kept moment's thumbnail card, used by the Pets dashboard and the keepsakes
// gallery. Its media stays flat (img + kind); open the moment via `moment(obsId)`.
export interface KeepsakeInsight {
  id: number
  obsId: number
  caption: string | null
  room: string
  img: string | null
  media: MediaKind
  at: string
}

export interface PetInsights {
  id: string
  name: string
  species: string
  description: string
  caveat: string | null
  seen: number
  glance: Chip[]
  note: Chip
  tiles: Tile[]
  spark: number[]
  habits: Habit[]
  rhythm: number[]
  rhythmMarks: number[]
  balance: Balance
  spots: Spot[]
  wellbeing: Wellbeing
  lastSeen: LastSeen | null
  recap: Recap | null
  keepsake: KeepsakeInsight | null
  monthSeen: number
  monthStats: StatPair[]
  anomaly: Chip | null
}

export interface Keepsake {
  id: number
  obsId: number
  petId: string | null
  caption: string | null
  at: string
}

export interface AskAnswer {
  answer: string
  refs: ObsView[]
}

// An owner correction of a mis-identified subject (exactly one target set). The
// moment id is in the request path.
export interface CorrectReq {
  petId?: string
  species?: string
  person?: boolean
  visiting?: boolean
}

// The outcome of the last background batch, for an honest refresh state.
export interface BatchOutcome {
  at: string
  status: string // ok | error
  note: string
}
export interface BatchStatus {
  running: boolean
  last: BatchOutcome | null
  // False when the app has not worked through all of that day's camera events yet, so the
  // day's moments and story are still incomplete. Compared against the day's end, not the
  // clock, so a caught-up app does not report itself behind between batches.
  caughtUp: boolean
}

// A flat, all-optional owner field edit of one moment (the id is in the path).
export interface EditReq {
  activity?: string
  wellbeing?: string
  description?: string
  whereAt?: string
  ate?: boolean
  drank?: boolean
  slept?: boolean
  played?: boolean
  groomed?: boolean
}

// Add a pet: the contract's { id, name, species, description, notes? }. `photo` is
// the optional avatar as raw base64 (no data-URL prefix).
export interface AddPetReq {
  id: string
  name: string
  species: string
  description: string
  notes?: string | null
  photo?: string
}
// A partial pet edit; an absent field keeps the current value. `photo` (raw base64)
// sets or replaces the avatar; `photoRemove` clears it.
export interface EditPetReq {
  name?: string
  species?: string
  description?: string
  notes?: string | null
  photo?: string
  photoRemove?: boolean
}

async function ok<T>(r: Response): Promise<T> {
  // ui.ts parseApiError knows the shape thrown here ("<status>: <raw body>") and pulls
  // the reason out of the nested { error: { code, message } } envelope when asked.
  if (!r.ok) throw new Error(`${r.status}: ${await r.text()}`)
  return r.json() as Promise<T>
}

// Every request aborts after a timeout so a stalled backend surfaces a clear error
// instead of a spinner that never resolves. LLM/whisper paths (ask, recap, refresh,
// transcribe) are slow by nature, so they get a far longer budget.
const DEFAULT_TIMEOUT = 20000
const LLM_TIMEOUT = 120000

const jsonHeaders = { 'content-type': 'application/json' }
const get = (path: string, timeout = DEFAULT_TIMEOUT) =>
  fetch(path, { signal: AbortSignal.timeout(timeout) })
const post = (path: string, body: unknown, timeout = DEFAULT_TIMEOUT) =>
  fetch(path, {
    method: 'POST',
    headers: jsonHeaders,
    body: JSON.stringify(body),
    signal: AbortSignal.timeout(timeout),
  })
const patch = (path: string, body: unknown, timeout = DEFAULT_TIMEOUT) =>
  fetch(path, {
    method: 'PATCH',
    headers: jsonHeaders,
    body: JSON.stringify(body),
    signal: AbortSignal.timeout(timeout),
  })
const del = (path: string, timeout = DEFAULT_TIMEOUT) =>
  fetch(path, { method: 'DELETE', signal: AbortSignal.timeout(timeout) })

// A query string from the present (non-empty) params, or '' when there are none.
function qs(params: Record<string, string | number | undefined>): string {
  const p = new URLSearchParams()
  for (const [k, v] of Object.entries(params)) {
    if (v !== undefined && v !== '') p.set(k, String(v))
  }
  const s = p.toString()
  return s ? `?${s}` : ''
}

export const api = {
  // Reads
  day: (d: string) => get(`/api/days/${encodeURIComponent(d)}`).then(ok<DayResponse>),
  moments: (q: MomentsQuery) => get(`/api/moments${qs({ ...q })}`).then(ok<MomentsPage>),
  moment: (id: number) => get(`/api/moments/${id}`).then(ok<ObsView>),
  overview: () => get('/api/overview').then(ok<Overview>),
  insights: (asOf?: string) =>
    get(`/api/pets/insights${asOf ? `?asOf=${encodeURIComponent(asOf)}` : ''}`)
      .then(ok<{ asOf: string; pets: PetInsights[] }>)
      .then((r) => r.pets),
  keepsakes: (pet?: string) =>
    get(`/api/keepsakes${pet ? `?pet=${encodeURIComponent(pet)}` : ''}`)
      .then(ok<{ items: KeepsakeInsight[] }>)
      .then((r) => r.items),
  status: () => get('/api/cameras/status').then(ok<Status>),
  cameras: () => get('/api/cameras').then(ok<DiscoveredCamera[]>),
  settings: () => get('/api/settings').then(ok<Profile>),
  // No day polls the daily (today) batch; a past-day ISO polls only that day's build.
  refreshStatus: (day?: string) =>
    get(`/api/refresh/status${day ? `?day=${encodeURIComponent(day)}` : ''}`).then(ok<BatchStatus>),
  cleanupInfo: () => get('/api/cleanup').then(ok<{ batchHours: number[]; nextRunAt: string | null }>),
  cleanupPreview: (days: number) =>
    get(`/api/cleanup/preview?days=${days}`).then(
      ok<{ days: number; effectiveDays: number; count: number }>,
    ),

  // Settings + roster
  saveSettings: (p: Profile) =>
    fetch('/api/settings', {
      method: 'PUT',
      headers: jsonHeaders,
      body: JSON.stringify(p),
      signal: AbortSignal.timeout(DEFAULT_TIMEOUT),
    }).then(ok<Profile>),
  addPet: (body: AddPetReq) => post('/api/pets', body).then(ok<Pet>),
  editPet: (id: string, body: EditPetReq) =>
    patch(`/api/pets/${encodeURIComponent(id)}`, body).then(ok<Pet>),
  // Draft (or enhance) a physical description from a photo. Stateless, so it works
  // during onboarding before the pet is saved; slow, so it gets the LLM budget.
  describePet: (body: { photo: string; species: string; description?: string }) =>
    post('/api/pets/describe', body, LLM_TIMEOUT).then(ok<{ description: string }>),
  archivePet: (id: string) =>
    post(`/api/pets/${encodeURIComponent(id)}/archive`, {}).then(ok<{ ok: boolean }>),
  unarchivePet: (id: string) =>
    post(`/api/pets/${encodeURIComponent(id)}/unarchive`, {}).then(ok<{ ok: boolean }>),
  deletePet: (id: string) => del(`/api/pets/${encodeURIComponent(id)}`).then(ok<{ ok: boolean }>),

  // Moment commands (the id is in the path)
  review: (ids: number[]) => post('/api/moments/review', { ids }).then(ok<{ ok: boolean }>),
  correct: (id: number, req: CorrectReq) =>
    post(`/api/moments/${id}/correction`, req).then(ok<{ ok: boolean }>),
  edit: (id: number, req: EditReq) => post(`/api/moments/${id}/edit`, req).then(ok<{ ok: boolean }>),
  revert: (id: number) => post(`/api/moments/${id}/revert`, {}).then(ok<{ ok: boolean }>),
  del: (id: number) => del(`/api/moments/${id}`).then(ok<{ ok: boolean }>),
  keep: (id: number, body: { petId?: string; caption?: string }) =>
    post(`/api/moments/${id}/keepsake`, body).then(ok<Keepsake>),
  unkeep: (keepsakeId: number) => del(`/api/keepsakes/${keepsakeId}`).then(ok<{ ok: boolean }>),
  transcribe: (id: number) =>
    fetch(`/api/moments/${id}/transcript`, {
      method: 'POST',
      signal: AbortSignal.timeout(LLM_TIMEOUT),
    }).then(ok<{ transcript: string }>),
  deleteMoments: (before?: string) =>
    post('/api/moments/delete', { before: before ?? null }).then(ok<{ deleted: number }>),

  // Agent + batch
  ask: (question: string) => post('/api/ask', { question }, LLM_TIMEOUT).then(ok<AskAnswer>),
  recap: (hours: number) => post('/api/recap', { hours }, LLM_TIMEOUT).then(ok<{ recap: string }>),
  // No day (or today) runs the full daily batch; a past-day ISO builds just that day.
  refresh: (day?: string) =>
    post('/api/refresh', { day: day ?? null }, LLM_TIMEOUT).then(ok<{ started: boolean }>),
  cleanupNow: () => post('/api/cleanup', {}).then(ok<{ collected: number; busy: boolean }>),
}

// The URL of a pet's stored avatar photo, cache-busted by its content token so a new
// photo re-fetches. Returns null when the pet has no photo (petPhoto is null).
export function petPhotoUrl(petId: string, token: string | null): string | null {
  return token
    ? `/api/pets/${encodeURIComponent(petId)}/photo?v=${encodeURIComponent(token)}`
    : null
}

// The avatar URL for a pet id looked up in a roster, or null when the pet is absent
// or has no photo. For screens whose view model carries a pet id but not the token.
export function photoUrlFor(pets: Pet[], id: string | null | undefined): string | null {
  if (!id) return null
  const p = pets.find((x) => x.petId === id)
  return p ? petPhotoUrl(p.petId, p.petPhoto) : null
}
