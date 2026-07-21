// Small cross-component user preferences (Svelte 5 runes), persisted to localStorage,
// mirroring theme.svelte. Kept separate from the server profile: these are per-device
// view choices, not account settings.

export type MomentOrder = 'newest' | 'oldest'

const ORDER_KEY = 'pr-moment-order'

export const prefs = $state({
  // How the Moments list is ordered. Defaults to newest-first (most people want the
  // latest moment on top); the toggle lets anyone flip to oldest-first.
  momentOrder: (localStorage.getItem(ORDER_KEY) === 'oldest' ? 'oldest' : 'newest') as MomentOrder,
})

export function toggleMomentOrder() {
  prefs.momentOrder = prefs.momentOrder === 'newest' ? 'oldest' : 'newest'
  localStorage.setItem(ORDER_KEY, prefs.momentOrder)
}
