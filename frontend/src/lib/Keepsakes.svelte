<script lang="ts">
  import { api, type KeepsakeInsight, type Pet } from './api'
  import Thumb from './Thumb.svelte'
  import Paw from './Paw.svelte'
  import GearButton from './GearButton.svelte'
  import { openLightbox } from './lightbox.svelte'
  import { fmtWhen, friendlyError } from './ui'

  let { pets, onnav }: { pets: Pet[]; onnav: (s: string) => void } = $props()

  let items = $state<KeepsakeInsight[]>([])
  let loaded = $state(false)
  let error = $state('')

  async function load() {
    error = ''
    try {
      items = await api.keepsakes()
    } catch (e) {
      error = friendlyError(e)
    } finally {
      loaded = true
    }
  }
  load()

  // A keepsake card only carries an obsId + thumbnail, so the full moment is fetched
  // on demand and opened in the lightbox.
  async function openMoment(obsId: number) {
    try {
      const obs = await api.moment(obsId)
      openLightbox([obs], obs.id, pets, load)
    } catch (e) {
      error = friendlyError(e)
    }
  }
</script>

<div class="fade px-[18px] pt-[14px] pb-[120px] lg:mx-auto lg:max-w-[1100px] lg:px-[32px] lg:pt-[24px] lg:pb-[60px]">
  <div class="mb-[12px] flex items-center justify-between gap-[10px]">
    <div class="font-head text-[22px] font-semibold" style="color:var(--text)">Keepsakes</div>
    <span class="lg:hidden"><GearButton {onnav} /></span>
  </div>
  <div class="mb-[14px] text-[13.5px]" style="color:var(--muted)">The moments you chose to keep for good.</div>

  {#if error}<p class="mb-3 text-[13px]" style="color:#e26d5c">{error}</p>{/if}

  {#if !loaded}
    <div class="flex justify-center py-[40px]" style="color:var(--accent);opacity:.5"><Paw size={30} /></div>
  {:else if items.length === 0}
    <div class="rounded-[22px] border border-dashed p-[34px] text-center" style="background:var(--surface);border-color:var(--line)">
      <div class="mb-[10px] flex justify-center" style="color:var(--accent);opacity:.5"><Paw size={36} /></div>
      <div class="mx-auto max-w-[240px] text-[13.5px] leading-[1.5]" style="color:var(--muted)">No keepsakes yet. Open a moment and tap Keep to save it here.</div>
    </div>
  {:else}
    <div class="grid grid-cols-2 gap-[11px] lg:grid-cols-4 xl:grid-cols-5">
      {#each items as k (k.id)}
        <button onclick={() => openMoment(k.obsId)} class="tappable fade overflow-hidden rounded-[18px] border text-left" style="background:var(--surface);border-color:var(--line);color:inherit">
          <div class="relative h-[120px] overflow-hidden">
            <Thumb img={k.img} media={k.media} room={k.room} pawSize={30} />
          </div>
          <div class="p-[10px]">
            <div class="truncate text-[11.5px] font-extrabold" style="color:var(--text)">{k.caption ?? k.room}</div>
            <div class="mt-[2px] text-[11px]" style="color:var(--muted)">{fmtWhen(k.at)}</div>
          </div>
        </button>
      {/each}
    </div>
  {/if}
</div>
