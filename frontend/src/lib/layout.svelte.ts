// Reactive viewport breakpoints, so the desktop shell and the screens that regroup into
// columns share one source of truth. Seeded synchronously from matchMedia so the very first
// render is already correct, with no flash of the mobile layout, then kept live on each
// media query's change event. Uses the class-with-runes form, as day.svelte.ts does, so the
// derived flags stay reactive across module boundaries.

class LayoutStore {
  // >= 1024px: the desktop shell, where the sidebar replaces the bottom tab bar
  // and content un-caps from the 480px phone column.
  isDesktop = $state(false)
  // >= 1280px: enough room for the Today / Pets / Moments two-column regroupings.
  isWide = $state(false)

  constructor() {
    this.watch('(min-width: 1024px)', (m) => (this.isDesktop = m))
    this.watch('(min-width: 1280px)', (m) => (this.isWide = m))
  }

  // Track one media query: seed now, then follow it. A guard keeps this safe if
  // ever evaluated without a DOM (e.g. a non-browser import).
  watch(query: string, set: (matches: boolean) => void) {
    if (typeof window === 'undefined' || !window.matchMedia) return
    const mq = window.matchMedia(query)
    set(mq.matches)
    mq.addEventListener('change', (e) => set(e.matches))
  }
}

export const layout = new LayoutStore()
