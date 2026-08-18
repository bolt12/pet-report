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
  // The sighting's index within its moment. This is how a correction addresses one
  // subject: a frame holding two cats has refs at 0 and 1, and naming one leaves the
  // other alone.
  ix: number
  petId: string | null
  label: string
  species: string | null
  person: boolean
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

// The vocabularies the server accepts. Unions rather than `string`, so a link built
// from a value the server cannot parse fails to compile instead of returning an empty
// page. The server rejects an unknown token with a 400 naming the legal set.
// `person` is a human; `visiting` is an animal the owner marked as not theirs. They were
// both called "visitor" in different places, which made a card saying "Visitor" impossible
// to read: it could mean either.
export type SubjectSel =
  | { kind: 'pet'; petId: string }
  | { kind: 'species'; species: string }
  | { kind: 'person' }
  | { kind: 'visiting' }

export type ActivityValue =
  | 'sleeping' | 'resting' | 'sitting' | 'standing' | 'walking' | 'running'
  | 'jumping' | 'playing' | 'eating' | 'drinking' | 'grooming' | 'eliminating'
  | 'alert' | 'absent' | 'unclear'

// One per projected behaviour column. Every per-pet tile counts one of these, so a tile
// links on the same word it counted rather than translating to a different facet.
export type BehaviourValue =
  | 'ate' | 'drank' | 'slept' | 'played' | 'groomed' | 'eliminated'
  | 'rest' | 'active' | 'concern'

export type WellbeingValue = 'normal' | 'concerning' | 'unclear'
export type MediaValue = 'photo' | 'clip' | 'audio'
export type ReviewValue = 'reviewed' | 'unreviewed' | 'needs-look'
export type TimeOfDayValue = 'morning' | 'afternoon' | 'evening' | 'night'
export type SortValue = 'asc' | 'desc'

// Render a subject selector as the `subject` query value the server parses.
export function subjectParam(s: SubjectSel): string {
  switch (s.kind) {
    case 'pet':
      return `pet:${s.petId}`
    case 'species':
      return `species:${s.species}`
    default:
      return s.kind
  }
}

// The facets of the moments browse, all optional (docs/api-contract.md 3.1).
export interface MomentsQuery {
  from?: string
  to?: string
  // Several AND together: a moment must hold every subject named. That is what makes
  // "Mochi and a person" a question you can ask.
  subject?: SubjectSel[]
  activity?: ActivityValue
  behaviour?: BehaviourValue
  wellbeing?: WellbeingValue
  room?: string
  // Raw camera ids. A favourite-spot link carries these because its room text is a
  // display label that may not match any saved room.
  camera?: string[]
  media?: MediaValue
  timeOfDay?: TimeOfDayValue
  review?: ReviewValue
  search?: string
  sort?: SortValue
  cursor?: string
  limit?: number
}

// What a link into Review means: the query it wants opened.
//
// This is a MomentsQuery minus the paging fields, deliberately, so a badge's count and the
// list its link opens are built from ONE value rather than two pieces of code that have to
// agree. The previous shape was a parallel set of loose string arrays (`who`, `act`) that
// each screen translated into facets by hand, and every translation was a chance to name a
// set the server could not answer. `who: ['visitor']` was one: it became `pet=visitor`,
// which matched nothing.
//
// `from`/`to` are ISO dates that switch Review into range mode; the rest seed the facets.
export type ReviewPreset = Omit<MomentsQuery, 'cursor' | 'limit' | 'sort' | 'search'>

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
  // A DISPLAY label. It may be a title-cased camera id when that camera is missing from
  // the profile or its room was saved blank, and no saved room label ever equals that, so
  // this must not be used as a query value. Filter on `cameras` instead.
  room: string
  cameras: string[]
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
// Tagged, so "a pet and a visitor at once" cannot be expressed. The server used to take
// four optional fields and settle any combination by precedence.
// A subject the owner says was present but the model did not report.
export type AddSightingReq = { kind: 'person' } | { kind: 'species'; species: string }

export type CorrectReq =
  | { kind: 'pet'; petId: string }
  | { kind: 'species'; species: string }
  | { kind: 'person' }
  | { kind: 'visiting' }

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

// A query string from the present (non-empty) params, or '' when there are none. An
// array value repeats its key, which is how the server reads `camera`.
function qs(params: Record<string, string | number | string[] | undefined>): string {
  const p = new URLSearchParams()
  for (const [k, v] of Object.entries(params)) {
    if (v === undefined || v === '') continue
    if (Array.isArray(v)) v.forEach((x) => p.append(k, x))
    else p.set(k, String(v))
  }
  const s = p.toString()
  return s ? `?${s}` : ''
}

// The moments query as wire params. The one place a MomentsQuery becomes a URL, so the
// structured `subject` selector is rendered in exactly one spot.
export function momentsParams(q: MomentsQuery): Record<string, string | number | string[] | undefined> {
  const { subject, ...rest } = q
  return { ...rest, subject: subject?.length ? subject.map(subjectParam) : undefined }
}

export const api = {
  // Reads
  day: (d: string) => get(`/api/days/${encodeURIComponent(d)}`).then(ok<DayResponse>),
  moments: (q: MomentsQuery) => get(`/api/moments${qs(momentsParams(q))}`).then(ok<MomentsPage>),
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
  // A subject the model missed. `species` is what a camera can perceive; naming the
  // individual is a separate correction against the new sighting.
  addSighting: (id: number, req: AddSightingReq) =>
    post(`/api/moments/${id}/sightings`, req).then(ok<{ ok: boolean }>),
  // A subject the model invented. The server renumbers the identity overrides with it.
  removeSighting: (id: number, ix: number) =>
    del(`/api/moments/${id}/sightings/${ix}`).then(ok<{ ok: boolean }>),
  // Both address one sighting by index, so a moment with several subjects can have each
  // named and edited on its own.
  correct: (id: number, ix: number, req: CorrectReq) =>
    post(`/api/moments/${id}/sightings/${ix}/correction`, req).then(ok<{ ok: boolean }>),
  edit: (id: number, ix: number, req: EditReq) =>
    post(`/api/moments/${id}/sightings/${ix}/edit`, req).then(ok<{ ok: boolean }>),
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
