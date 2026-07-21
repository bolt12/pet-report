<script lang="ts">
  // The "what to watch for" report-emphasis toggles, shared by the setup wizard and
  // the Settings editor so the field list, styling, and toggle logic can't drift.
  // Two-way bound: the parent keeps the emphasis map, this owns flipping a key.
  import Toggle from './Toggle.svelte'
  import { EMPHASIS } from './emphasis'

  let { emphasis = $bindable() }: { emphasis: Record<string, boolean> } = $props()
  const toggle = (key: string) => (emphasis = { ...emphasis, [key]: !emphasis[key] })
</script>

<div class="rounded-[20px] border px-[16px] py-[6px]" style="background:var(--surface);border-color:var(--line)">
  {#each EMPHASIS as e (e.key)}
    <div class="flex items-center justify-between gap-[12px] border-b py-[12px] last:border-b-0" style="border-color:var(--line)">
      <div class="min-w-0 flex-1">
        <div class="text-[14px] font-bold" style="color:var(--text)">{e.label}</div>
        <div class="mt-[1px] text-[11.5px]" style="color:var(--faint)">{e.sub}</div>
      </div>
      <Toggle checked={emphasis[e.key]} onToggle={() => toggle(e.key)} label={e.label} />
    </div>
  {/each}
</div>
<div class="mt-[7px] px-[4px] text-[11px] leading-[1.4]" style="color:var(--faint)">The report can only cover what the cameras actually see.</div>
