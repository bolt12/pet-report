// The shared "selected day", so Today, Moments, and Pets agree on which day is
// shown. offset 0 = today, growing into the past. Unlike the plain-object stores
// (lightbox, theme), this one carries derived values (iso, label), so it uses the
// class-with-runes form, which keeps them reactive across module boundaries.

import { isoForOffset, dayLabel, offsetForIso } from './ui'

class DayStore {
  offset = $state(0)
  // Oldest reachable day. Defaults to the prior 30 days so the back arrow is never dead on
  // a cold start, then narrows to the first logged day once /api/overview reports it.
  earliestOffset = $state(30)

  iso = $derived(isoForOffset(this.offset))
  label = $derived(dayLabel(this.offset))

  // Older: step back in time, stopping at the earliest logged day.
  prevDay() {
    if (this.offset < this.earliestOffset) this.offset += 1
  }
  // Newer: step toward today, stopping at today.
  nextDay() {
    if (this.offset > 0) this.offset -= 1
  }
  resetToday() {
    this.offset = 0
  }
  // Seed the earliest reachable day from the first logged local date (YYYY-MM-DD).
  setEarliest(iso: string | null | undefined) {
    if (iso) this.earliestOffset = offsetForIso(iso)
  }
}

export const day = new DayStore()
