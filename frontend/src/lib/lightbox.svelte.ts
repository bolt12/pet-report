// A shared lightbox controller: any screen can open a moment (with its sibling
// list for prev/next) and supply a reload callback that runs after a mutation.

import type { ObsView, Pet } from './api'

export const lb = $state<{
  list: ObsView[]
  index: number
  pets: Pet[]
  onChanged: () => void
}>({
  list: [],
  index: -1,
  pets: [],
  onChanged: () => {},
})

export function openLightbox(list: ObsView[], id: number, pets: Pet[], onChanged: () => void) {
  const i = list.findIndex((o) => o.id === id)
  lb.list = list
  lb.index = i < 0 ? 0 : i
  lb.pets = pets
  lb.onChanged = onChanged
}

export function closeLightbox() {
  lb.index = -1
  lb.list = []
}

export function stepLightbox(delta: number) {
  const next = lb.index + delta
  if (next >= 0 && next < lb.list.length) lb.index = next
}
