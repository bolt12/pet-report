<script lang="ts">
  import { day } from './day.svelte'
</script>

<!-- Shared day navigator: prev/next arrows around the selected-day label. Drives
     the shared day store, so Today, Moments, and Pets stay in step. -->
<div class="mb-[14px] flex items-center justify-center gap-[14px]">
  <button onclick={() => day.nextDay()} disabled={day.offset === 0} class="flex h-[32px] w-[32px] items-center justify-center rounded-full border text-[16px]" style="border-color:var(--line);background:transparent;color:var(--muted);opacity:{day.offset > 0 ? 1 : 0.3}" aria-label="Newer day">‹</button>
  <span class="font-head min-w-[96px] text-center text-[15px] font-semibold" style="color:var(--text)">{day.label}</span>
  <button onclick={() => day.prevDay()} disabled={day.offset >= day.earliestOffset} class="flex h-[32px] w-[32px] items-center justify-center rounded-full border text-[16px]" style="border-color:var(--line);background:transparent;color:var(--muted);opacity:{day.offset >= day.earliestOffset ? 0.3 : 1}" aria-label="Older day">›</button>
</div>
