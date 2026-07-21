// Report-emphasis groups: each maps a friendly toggle to the backend topic keys
// stored in Profile.report.topics. Shared by the setup wizard and the Settings
// editor so the two can't drift, and so both write the same topic strings.

export interface EmphasisGroup {
  key: string
  label: string
  sub: string
  topics: string[]
}

// Only what a camera can actually report. Meals/water and litter live off-camera
// most of the time, so we don't offer them as "highlights" the report can't keep.
export const EMPHASIS: EmphasisGroup[] = [
  { key: 'rest', label: 'Rest & sleep', sub: 'naps, favourite spots', topics: ['sleep'] },
  { key: 'play', label: 'Play & zoomies', sub: 'active, playful moments', topics: ['play'] },
  { key: 'grooming', label: 'Grooming', sub: 'washing, brushing', topics: ['grooming'] },
  { key: 'visitors', label: 'Visitors & people', sub: 'who came by', topics: ['visitors'] },
  { key: 'sounds', label: 'Sounds', sub: 'barks, meows, the doorbell', topics: ['sounds'] },
  { key: 'outdoors', label: 'Time outdoors', sub: 'garden, cat-flap trips', topics: ['outdoors'] },
  { key: 'health', label: 'Health & mobility', sub: 'limps, changes to watch', topics: ['health'] },
]

// The default emphasis on first-run setup (before any profile exists).
export const DEFAULT_EMPHASIS = new Set(['rest', 'visitors', 'sounds', 'health'])

// A group is "on" when any of its topics is present in the saved topic list.
export function topicsToEmphasis(topics: string[]): Record<string, boolean> {
  const t = new Set(topics)
  return Object.fromEntries(EMPHASIS.map((g) => [g.key, g.topics.some((x) => t.has(x))]))
}

// The flat, de-duplicated topic list for the enabled groups.
export function emphasisToTopics(emphasis: Record<string, boolean>): string[] {
  return [...new Set(EMPHASIS.filter((g) => emphasis[g.key]).flatMap((g) => g.topics))]
}
