// A shared live-view controller: Today opens a camera's live still, and the
// app-level Back handler can close it. Mirrors lightbox.svelte.ts (a $state
// object plus open/close functions, with `open:false` as the closed sentinel).

export const live = $state<{ open: boolean; cam: string; room: string; online: boolean }>({
  open: false,
  cam: '',
  room: '',
  // Whether the camera was online per Today's overview; a stale last-known frame
  // means onerror alone cannot tell an offline camera apart, so the opener carries it.
  online: true,
})

export function openLive(cam: string, room: string, online: boolean) {
  live.cam = cam
  live.room = room
  live.online = online
  live.open = true
}

export function closeLive() {
  live.open = false
  live.cam = ''
  live.room = ''
  live.online = true
}
