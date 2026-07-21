<script lang="ts">
  import Paw from './Paw.svelte'
  import { roomTint } from './ui'
  import type { MediaKind } from './api'

  let {
    img,
    media,
    room,
    pawSize = 34,
  }: { img: string | null; media: MediaKind; room: string; pawSize?: number } = $props()

  let broken = $state(false)
  let showImg = $derived(!!img && media !== 'expired' && media !== 'audio' && !broken)
</script>

<div class="absolute inset-0" style="background:{roomTint(room)}"></div>

{#if showImg}
  <img
    src={img}
    alt=""
    class="absolute inset-0 h-full w-full object-cover"
    onerror={() => (broken = true)}
  />
{:else}
  <div class="absolute inset-0 flex items-center justify-center" style="color:rgba(255,246,236,0.2)">
    <Paw size={pawSize} />
  </div>
{/if}

{#if media === 'clip'}
  <div class="absolute inset-0 flex items-center justify-center">
    <span
      class="flex items-center justify-center rounded-full"
      style="width:30px;height:30px;background:rgba(20,14,10,0.42)"
    >
      <span
        style="width:0;height:0;border-left:9px solid rgba(255,246,236,0.92);border-top:6px solid transparent;border-bottom:6px solid transparent;margin-left:2px"
      ></span>
    </span>
  </div>
{:else if media === 'audio'}
  <div class="absolute inset-0 flex items-center justify-center gap-[3px]">
    <span
      class="rounded-[2px]"
      style="width:3px;height:12px;background:rgba(255,246,236,0.8);animation:petBar 1s ease infinite"
    ></span>
    <span
      class="rounded-[2px]"
      style="width:3px;height:20px;background:rgba(255,246,236,0.8);animation:petBar 1s ease .15s infinite"
    ></span>
    <span
      class="rounded-[2px]"
      style="width:3px;height:9px;background:rgba(255,246,236,0.8);animation:petBar 1s ease .3s infinite"
    ></span>
  </div>
{:else if media === 'expired'}
  <div
    class="absolute inset-0 flex items-center justify-center px-2 text-center"
    style="background:rgba(25,19,15,0.55);color:rgba(255,246,236,0.6);font-size:10px"
  >
    clip tidied away
  </div>
{/if}
