<script lang="ts">
  // The four primary-nav glyphs, shared by the mobile bottom tabs and the desktop
  // sidebar so the shapes stay identical in both places. `kind` is the destination
  // key; anything unrecognised falls back to the Moments ring.
  import Paw from './Paw.svelte'

  let { kind, pawSize = 22 }: { kind: string; pawSize?: number } = $props()
</script>

{#if kind === 'today'}
  <Paw size={pawSize} />
{:else if kind === 'pets'}
  <span class="flex items-center">
    <span class="h-[13px] w-[13px] rounded-full" style="background:currentColor"></span>
    <span class="-ml-[5px] h-[13px] w-[13px] rounded-full opacity-[0.55]" style="background:currentColor"></span>
  </span>
{:else if kind === 'ask'}
  <span class="h-[16px] w-[19px]" style="background:currentColor;border-radius:9px 9px 9px 2px"></span>
{:else}
  <span class="h-[17px] w-[17px] rounded-full" style="border:2.6px solid currentColor"></span>
{/if}
