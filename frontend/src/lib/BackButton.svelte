<script lang="ts">
  // The round ‹ back button shared verbatim by the Settings and Manage data headers,
  // so the shape and styling can't drift between them.
  let { onclick }: { onclick: () => void } = $props()
</script>

<button {onclick} class="flex h-[38px] w-[38px] items-center justify-center rounded-full border text-[18px]" style="border-color:var(--line);background:var(--surface);color:var(--text)" aria-label="Back">‹</button>
