<script lang="ts">
  // The settings gear, shared across every tab's header so Settings is one tap
  // from anywhere (not only from Today). Styling matches Today's original gear.
  let { onnav }: { onnav: (s: string) => void } = $props()
</script>

<button
  onclick={() => onnav('settings')}
  class="flex h-[40px] w-[40px] flex-shrink-0 items-center justify-center rounded-full border text-[17px]"
  style="border-color:var(--line);background:var(--surface);color:var(--muted)"
  aria-label="Settings">⚙</button>
