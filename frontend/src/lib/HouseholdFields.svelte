<script lang="ts">
  // The home-context fields (cat flap, neighbour's cat, feed times), shared by the
  // setup wizard and the Settings editor. Two-way bound so the parent's profile
  // state stays the single source of truth.
  import Toggle from './Toggle.svelte'

  let {
    catFlap = $bindable(),
    neighbourCat = $bindable(),
    feedTimes = $bindable(),
  }: { catFlap: boolean; neighbourCat: boolean; feedTimes: string } = $props()
</script>

<div class="rounded-[20px] border px-[16px] pt-[6px] pb-[16px]" style="background:var(--surface);border-color:var(--line)">
  <div class="flex items-center justify-between gap-[12px] border-b py-[12px]" style="border-color:var(--line)">
    <div class="min-w-0 flex-1"><div class="text-[14px] font-bold" style="color:var(--text)">There's a cat flap</div><div class="mt-[1px] text-[11.5px]" style="color:var(--faint)">So comings and goings look normal</div></div>
    <Toggle checked={catFlap} onToggle={() => (catFlap = !catFlap)} label="Cat flap" />
  </div>
  <div class="flex items-center justify-between gap-[12px] border-b py-[12px]" style="border-color:var(--line)">
    <div class="min-w-0 flex-1"><div class="text-[14px] font-bold" style="color:var(--text)">A neighbour's cat visits</div><div class="mt-[1px] text-[11.5px]" style="color:var(--faint)">So I won't mistake them for yours</div></div>
    <Toggle checked={neighbourCat} onToggle={() => (neighbourCat = !neighbourCat)} label="Neighbour's cat" />
  </div>
  <div class="pt-[14px]">
    <div class="text-[14px] font-bold" style="color:var(--text)">Usual feed times</div>
    <div class="mt-[1px] mb-[9px] text-[11.5px]" style="color:var(--faint)">So the report can note who's around at mealtimes</div>
    <input bind:value={feedTimes} placeholder="e.g. 7:30am, 6pm" class="w-full rounded-[14px] border px-[15px] py-[11px] text-[14px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
  </div>
</div>
