// Presentation helpers shared across screens: avatars, time/date formatting,
// day navigation, and chip styling. Pure, no reactive state.

import type { ChipKind, Pet } from './api'

// A pet is "present" until it's archived. An archived pet keeps its data and surfaces only
// in Manage data. Shared, so the active/archived split can't drift between the setup editor,
// Settings, and Manage data.
export const isPresent = (p: Pet): boolean => !p.petArchivedAt
export const isArchived = (p: Pet): boolean => !!p.petArchivedAt

// Species-keyed avatar gradients (warm, distinct). Falls back to a neutral pair.
const PALETTE: Record<string, [string, string]> = {
  cat: ['#f0b389', '#d98b6f'],
  dog: ['#d6ad84', '#b58a63'],
  rabbit: ['#cdb0a0', '#b19484'],
  bird: ['#e8c07d', '#d3a15a'],
  small: ['#d8b48c', '#bd9068'],
  reptile: ['#a9bd8e', '#889c66'],
  fish: ['#a7b8b0', '#87988f'],
  horse: ['#c8a582', '#a97f5a'],
  other: ['#c3a9c0', '#a789a4'],
}

export function avatarGradient(species: string): string {
  const key = species.toLowerCase().split(' ')[0]
  const [a, b] = PALETTE[key] ?? PALETTE.other
  return `linear-gradient(145deg, ${a}, ${b})`
}

export function initial(name: string): string {
  return (name.trim()[0] ?? '?').toUpperCase()
}

// A stable id for a new record. Uses crypto.randomUUID where available, with a fallback for
// non-secure contexts like plain http over a LAN IP, where randomUUID is undefined and would
// throw, silently aborting the action.
export function uid(prefix = 'p'): string {
  const c = globalThis.crypto
  if (c && typeof c.randomUUID === 'function') return c.randomUUID()
  return `${prefix}-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`
}

export function greeting(d = new Date()): string {
  const h = d.getHours()
  if (h < 5) return 'Still up?'
  if (h < 12) return 'Good morning'
  if (h < 18) return 'Good afternoon'
  return 'Good evening'
}

export function longDate(d = new Date()): string {
  return d.toLocaleDateString(undefined, { weekday: 'long', day: 'numeric', month: 'long' })
}

// Day navigation: offset 0 = today, 1 = yesterday, ... maps to an ISO date the
// backend accepts (/api/days/YYYY-MM-DD), and a friendly label.
export function isoForOffset(offset: number): string {
  if (offset === 0) return 'today'
  const d = new Date()
  d.setDate(d.getDate() - offset)
  // Local calendar date, not toISOString, which is UTC and can land a day off near
  // midnight. The backend windows a day in its own local zone.
  const y = d.getFullYear()
  const m = String(d.getMonth() + 1).padStart(2, '0')
  const day = String(d.getDate()).padStart(2, '0')
  return `${y}-${m}-${day}`
}

// Like isoForOffset but always a concrete 'YYYY-MM-DD' (never 'today'), for date
// inputs, date-range params, and range labels.
export function ymdForOffset(offset: number): string {
  const d = new Date()
  d.setDate(d.getDate() - offset)
  const y = d.getFullYear()
  const m = String(d.getMonth() + 1).padStart(2, '0')
  const day = String(d.getDate()).padStart(2, '0')
  return `${y}-${m}-${day}`
}

// Local 'YYYY-MM-DD' of a timestamp (device-local calendar day), for linking a
// written time to the day its moment actually falls on.
export function ymdOf(iso: string): string {
  const d = new Date(iso)
  const y = d.getFullYear()
  const m = String(d.getMonth() + 1).padStart(2, '0')
  const day = String(d.getDate()).padStart(2, '0')
  return `${y}-${m}-${day}`
}

// A friendly short label ("7 Jul") for a 'YYYY-MM-DD' date, parsed as local.
export function fmtDate(ymd: string): string {
  const [y, m, d] = ymd.split('-').map(Number)
  if (!y || !m || !d) return ymd
  return new Date(y, m - 1, d).toLocaleDateString(undefined, { day: 'numeric', month: 'short' })
}

// --- time of day (local minutes since midnight) ----------------------------
// The half-width of the window a tapped time opens in Moments. One place to tune
// how tightly a written time ("8:14 am") lands on its moment.
export const TIME_LINK_RADIUS_MIN = 15

export const toMinutes = (hhmm: string): number => {
  const [h, m] = hhmm.split(':').map(Number)
  return (h || 0) * 60 + (m || 0)
}
// Clamped 'HH:MM' for a minute count (00:00..23:59).
export const fromMinutes = (n: number): string => {
  const c = Math.max(0, Math.min(1439, Math.round(n)))
  return `${String(Math.floor(c / 60)).padStart(2, '0')}:${String(c % 60).padStart(2, '0')}`
}
// Local minutes-since-midnight of a timestamp (for filtering ObsView.at by time).
export const localMinutesOf = (iso: string): number => {
  const d = new Date(iso)
  return d.getHours() * 60 + d.getMinutes()
}

// Minutes-since-midnight of a timestamp read in a specific IANA zone, so the time-of-day
// filter compares against the same zone the report's written times use rather than the
// device's. A null, empty or unusable `timeZone` falls through to localMinutesOf, so nothing
// changes unless the owner is travelling.
export const minutesInZone = (iso: string, timeZone?: string | null): number => {
  if (!timeZone) return localMinutesOf(iso)
  try {
    const parts = new Intl.DateTimeFormat(undefined, {
      timeZone,
      hour: '2-digit',
      minute: '2-digit',
      hour12: false,
    }).formatToParts(new Date(iso))
    const h = Number(parts.find((p) => p.type === 'hour')?.value)
    const m = Number(parts.find((p) => p.type === 'minute')?.value)
    // hour12:false can render midnight as "24"; normalise it back to 0.
    if (Number.isFinite(h) && Number.isFinite(m)) return (h % 24) * 60 + m
  } catch {
    // An unknown/unsupported zone name: fall back to device-local.
  }
  return localMinutesOf(iso)
}

// Inverse of isoForOffset: how many local days back a 'YYYY-MM-DD' date is from today, 0
// being today. Parsed as a local date rather than through new Date(iso), which is UTC and
// can land a day off, then diffed against local midnight today, rounded to absorb a short or
// long DST day, and clamped at 0.
export function offsetForIso(iso: string): number {
  const [y, m, d] = iso.split('-').map(Number)
  if (!y || !m || !d) return 0
  const then = new Date(y, m - 1, d).getTime()
  const now = new Date()
  const today = new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime()
  return Math.max(0, Math.round((today - then) / 86400000))
}

// The backend-proxied URL for a camera's latest still. `tick` is a cache-buster so a polled
// tile actually refreshes, and encodeURIComponent guards a camera name containing a hyphen
// or another escapable character, which Frigate allows.
export function frameSrc(cam: string, tick: number): string {
  return `/api/cameras/${encodeURIComponent(cam)}/frame?t=${tick}`
}

// The one place that knows the shape ok<T> throws, "<status>: <json body>". Pulls out the
// HTTP status, plus the message when the body is the { error: { code, message } } envelope.
function parseApiError(e: unknown): { status?: string; message?: string } {
  // Anchored to the start (ok<T> throws `${status}: ${body}`, stringified as
  // "Error: <status>: <body>"), so a number inside the body is never read as the status.
  const m = String(e).match(/^(?:Error:\s*)?(\d{3}):\s*(.*)$/s)
  if (!m) return {}
  let message: string | undefined
  try {
    const body = JSON.parse(m[2]) as { error?: { message?: unknown } }
    if (typeof body.error?.message === 'string' && body.error.message.trim()) message = body.error.message
  } catch {
    // body was not the JSON envelope; leave message undefined
  }
  return { status: m[1], message }
}

// Turn a thrown fetch or API error into a calm, user-facing line.
export function friendlyError(e: unknown): string {
  // A request that aborts on AbortSignal.timeout throws a DOMException named 'TimeoutError'
  // with a "signal timed out" message. Render that as something actionable rather than the
  // raw name or the generic fallback.
  if ((e instanceof DOMException && e.name === 'TimeoutError') || /TimeoutError|timed out/i.test(String(e)))
    return 'That took too long to respond. Please try again.'
  if (/Failed to fetch|NetworkError|load failed/i.test(String(e)))
    return "Couldn't reach the server. Check it's running and try again."
  const { status } = parseApiError(e)
  return status
    ? `The server hit a problem (${status}). Please try again in a moment.`
    : 'Something went wrong. Please try again.'
}

// Prefer the server's own message from the { error: { message } } envelope when it carries
// an actionable reason, such as "transcription is off in Frigate". Otherwise the generic
// line.
export function serverMessage(e: unknown): string {
  return parseApiError(e).message ?? friendlyError(e)
}

export function dayLabel(offset: number): string {
  if (offset === 0) return 'Today'
  if (offset === 1) return 'Yesterday'
  const d = new Date()
  d.setDate(d.getDate() - offset)
  return d.toLocaleDateString(undefined, { weekday: 'short', day: 'numeric', month: 'short' })
}

export function fmtTime(iso: string): string {
  try {
    return new Date(iso).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
  } catch {
    return ''
  }
}

// A moment's day and clock time together ("Today · 08:14 AM", "Sat, 8 Aug · 3:20 PM"),
// for a viewer that may show a moment from any day (keepsakes, a pet's history, Ask
// proof). Reuses dayLabel so a recent day reads relatively and an older one by date.
export function fmtWhen(iso: string): string {
  return `${dayLabel(offsetForIso(ymdOf(iso)))} · ${fmtTime(iso)}`
}

export function relTime(iso: string): string {
  const then = new Date(iso).getTime()
  const mins = Math.round((Date.now() - then) / 60000)
  if (mins < 1) return 'just now'
  if (mins < 60) return `${mins} minute${mins === 1 ? '' : 's'} ago`
  const hrs = Math.round(mins / 60)
  if (hrs < 24) return `about ${hrs} hour${hrs === 1 ? '' : 's'} ago`
  const days = Math.round(hrs / 24)
  return `${days} day${days === 1 ? '' : 's'} ago`
}

// Chip color pairs by kind: [background, foreground].
const CHIP: Record<string, [string, string]> = {
  good: ['rgba(163,192,143,0.16)', 'var(--good)'],
  watch: ['rgba(236,177,99,0.16)', 'var(--watch)'],
  info: ['rgba(183,168,192,0.16)', 'var(--unclear)'],
  plain: ['var(--surface2)', 'var(--muted)'],
}

// The chip foreground color alone (for text tinted by status, no pill).
export function chipFg(kind: ChipKind | string): string {
  return (CHIP[kind] ?? CHIP.info)[1]
}

// A full chip pill style (background + foreground), as an inline style string.
export function chipStyle(kind: ChipKind | string): string {
  const [bg, fg] = CHIP[kind] ?? CHIP.info
  return `background:${bg};color:${fg}`
}

// A soft, room-derived gradient for thumbnail placeholders (deterministic).
export function roomTint(room: string): string {
  let h = 0
  for (let i = 0; i < room.length; i++) h = (h * 31 + room.charCodeAt(i)) & 0xffff
  const hue = h % 360
  return `linear-gradient(150deg, hsl(${hue} 22% 22%), hsl(${(hue + 30) % 360} 24% 14%))`
}

// A service-connection status dot (reachable => sage + glow, else faint), shared
// by the setup Connect step and Settings so the styling can't drift.
export function connDotStyle(reachable: boolean): string {
  return reachable
    ? 'background:var(--good);box-shadow:0 0 8px rgba(163,192,143,0.6)'
    : 'background:var(--faint);box-shadow:none'
}
export function connTextColor(reachable: boolean): string {
  return reachable ? 'var(--good)' : 'var(--faint)'
}

export const WELLBEING_DOT: Record<string, string> = {
  normal: 'var(--good)',
  concerning: 'var(--watch)',
  unclear: 'var(--unclear)',
  none: 'var(--faint)',
}

// The tracked per-subject behaviour flags, matching the backend chip labels
// (Domain/View.hs apChips) and Behaviors record. Kept in one place so the edit
// toggles and any chip-label checks stay in step.
export type BehaviourFlag = 'ate' | 'drank' | 'slept' | 'played' | 'groomed'
export const BEHAVIOUR_FLAGS: readonly { key: BehaviourFlag; label: string }[] = [
  { key: 'ate', label: 'Ate' },
  { key: 'drank', label: 'Drank' },
  { key: 'slept', label: 'Slept' },
  { key: 'played', label: 'Played' },
  { key: 'groomed', label: 'Groomed' },
]
