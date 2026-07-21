<script lang="ts">
  import { untrack } from 'svelte'
  import Paw from './Paw.svelte'
  import { roomTint, frameSrc } from './ui'

  let {
    cam,
    room,
    online,
    onclose,
  }: { cam: string; room: string; online: boolean; onclose: () => void } = $props()

  // `online` is a one-time seed (the overlay is re-mounted per open, carrying the
  // camera's status at open time), so read it untracked to seed the state below.
  const startOnline = untrack(() => online)

  let tick = $state(0)
  // Frigate serves a stale last-known frame for an OFFLINE camera, so onerror alone
  // cannot tell offline from a transient hiccup. Seed `broken` from the opener's
  // online flag: an offline camera opens straight to the placeholder and never
  // fetches a (misleadingly live-looking) stale frame.
  let broken = $state(!startOnline)
  let loaded = $state(false)
  // The 1s refresh runs only while a frame is actually loading; a dead or offline
  // camera is not polled. A successful onload turns it on; onerror / a retry turn it
  // off, so we never hammer Frigate for a camera nobody can see.
  let polling = $state(startOnline)

  // Refresh the still roughly every second by bumping a cache-busting param while
  // polling and the tab is visible; paused otherwise so we don't keep hitting
  // Frigate for frames nobody is looking at, or for a camera that is down.
  $effect(() => {
    if (!polling) return
    let id: ReturnType<typeof setInterval> | null = null
    const start = () => {
      if (id === null) id = setInterval(() => (tick += 1), 1000)
    }
    const stop = () => {
      if (id !== null) {
        clearInterval(id)
        id = null
      }
    }
    const onVis = () => (document.hidden ? stop() : start())
    onVis()
    document.addEventListener('visibilitychange', onVis)
    return () => {
      stop()
      document.removeEventListener('visibilitychange', onVis)
    }
  })

  // Re-attempt a single frame: clear the placeholder and bump the cache-buster once.
  // Continuous polling resumes only if that frame actually loads (see onload).
  function retry() {
    broken = false
    loaded = false
    polling = false
    tick += 1
  }

  let src = $derived(frameSrc(cam, tick))

  function onKey(e: KeyboardEvent) {
    if (e.key === 'Escape') onclose()
  }
</script>

<svelte:window onkeydown={onKey} />

<div
  class="fade fixed inset-0 z-40 flex flex-col"
  style="background:rgba(15,11,8,0.95);backdrop-filter:blur(6px)"
>
  <div class="flex items-center justify-between px-[18px] pt-[18px] pb-[12px]">
    <button
      onclick={onclose}
      class="flex h-[38px] w-[38px] items-center justify-center rounded-full text-[18px] text-white"
      style="background:rgba(255,246,236,0.14)"
      aria-label="Close">✕</button
    >
    {#if online && !broken}
      <span class="inline-flex items-center gap-[6px] text-[12px] font-extrabold tracking-wider uppercase" style="color:rgba(255,246,236,0.72)">
        <span class="h-[7px] w-[7px] rounded-full" style="background:#e26d5c;box-shadow:0 0 8px #e26d5c;animation:petPulse 1.8s ease infinite"></span>
        Live
      </span>
    {:else}
      <span class="inline-flex items-center rounded-full px-[8px] py-[2px] text-[9px] font-extrabold tracking-wider uppercase" style="background:rgba(20,14,10,0.4);color:rgba(255,246,236,0.55)">Offline</span>
    {/if}
    <span class="w-[38px]"></span>
  </div>

  <div class="relative flex flex-1 items-center justify-center px-[18px] pb-[28px]">
    <!-- Backdrop closes on click; the image card sits above it, so tapping the
         picture does not dismiss the view. -->
    <button onclick={onclose} class="absolute inset-0" style="background:none;border:none" aria-label="Close live view"></button>
    <div class="relative w-full overflow-hidden rounded-[22px]" style="aspect-ratio:4/3;max-width:min(92vw,760px);box-shadow:0 20px 50px -10px rgba(0,0,0,0.6)">
      <div class="absolute inset-0" style="background:{roomTint(room)}"></div>
      {#if !loaded && !broken}
        <div class="absolute inset-0 flex flex-col items-center justify-center gap-[9px]" style="color:rgba(255,246,236,0.55)">
          <span class="h-[26px] w-[26px] rounded-full" style="border:3px solid rgba(255,246,236,0.22);border-top-color:rgba(255,246,236,0.7);animation:petSpin .8s linear infinite"></span>
          <span class="text-[12px] font-bold">Connecting…</span>
        </div>
      {/if}
      {#if broken}
        <!-- Tapping the placeholder re-attempts a frame (as does the retry button). -->
        <button onclick={retry} class="absolute inset-0 flex flex-col items-center justify-center gap-[8px]" style="background:none;border:none;color:rgba(255,246,236,0.4)" aria-label="Retry">
          <Paw size={44} />
          <span class="text-[12px] font-bold" style="color:rgba(255,246,236,0.55)">camera offline</span>
          <span class="mt-[4px] rounded-full px-[13px] py-[6px] text-[11.5px] font-extrabold" style="background:rgba(255,246,236,0.14);color:rgba(255,246,236,0.82)">Retry</span>
        </button>
      {/if}
      <!-- Only mounted while trying (not broken), so an offline camera never fetches
           a stale frame; retry() clears `broken` to re-mount and re-attempt. -->
      {#if !broken}
        <img
          {src}
          alt=""
          class="absolute inset-0 h-full w-full object-cover"
          style="opacity:{loaded ? 1 : 0};transition:opacity .18s"
          onload={() => ((loaded = true), (polling = true))}
          onerror={() => ((broken = true), (polling = false))}
        />
      {/if}
      <div class="absolute right-0 bottom-0 left-0 h-[48px]" style="background:linear-gradient(0deg,rgba(20,14,10,0.62),transparent)"></div>
      <span class="absolute bottom-[11px] left-[14px] font-head text-[15px] font-semibold" style="color:rgba(255,246,236,0.96)">{room}</span>
    </div>
  </div>
</div>
