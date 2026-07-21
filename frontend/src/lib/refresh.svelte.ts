// Per-day refresh tracking, so each day's Refresh button reflects only its OWN
// in-flight rebuild. Keyed by concrete ISO date (never the "today" sentinel), so a
// mid-refresh day switch can never flip the wrong button. Reassigns a $state record for
// reactivity across module boundaries, matching the class-with-runes form in day.svelte.

class RefreshStore {
  // ISO dates with a rebuild in flight.
  busy = $state<Record<string, true>>({})
  // The last ISO date whose refresh finished with a real "ok" run, for a per-day
  // "Just updated" note that does not bleed onto other days.
  updatedIso = $state<string | null>(null)

  isRefreshing(iso: string): boolean {
    return this.busy[iso] === true
  }
  // Whether some OTHER day is refreshing, for a gentle "one at a time" hint.
  otherRefreshing(iso: string): boolean {
    return Object.keys(this.busy).some((k) => k !== iso)
  }
  start(iso: string) {
    this.busy = { ...this.busy, [iso]: true }
    if (this.updatedIso === iso) this.updatedIso = null
  }
  stop(iso: string) {
    const rest = { ...this.busy }
    delete rest[iso]
    this.busy = rest
  }
}

export const refreshes = new RefreshStore()
