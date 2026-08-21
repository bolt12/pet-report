<script lang="ts">
  import {
    api,
    photoUrlFor,
    type BehaviourValue,
    type Pet,
    type PetInsights,
    type ReviewPreset,
  } from './api'
  import Avatar from './Avatar.svelte'
  import Paw from './Paw.svelte'
  import Thumb from './Thumb.svelte'
  import Chip from './Chip.svelte'
  import DayNav from './DayNav.svelte'
  import { openLightbox } from './lightbox.svelte'
  import GearButton from './GearButton.svelte'
  import { day } from './day.svelte'
  import { layout } from './layout.svelte'
  import { avatarGradient, chipStyle, relTime, friendlyError, ymdForOffset } from './ui'

  let { initialPetId, pets, onnav }: { initialPetId?: string; pets: Pet[]; onnav: (s: string, arg?: string | ReviewPreset) => void } = $props()

  let list = $state<PetInsights[]>([])
  let selected = $state<string | null>(null)
  let error = $state('')
  let loaded = $state(false)

  async function load() {
    error = ''
    try {
      // Insights for the selected day; today uses the plain endpoint.
      list = await api.insights(day.offset === 0 ? undefined : day.iso)
      if (!selected) selected = initialPetId ?? null
      if (!selected || !list.some((p) => p.id === selected)) selected = list[0]?.id ?? null
    } catch (e) {
      error = friendlyError(e)
    } finally {
      loaded = true
    }
  }
  // Reload when the shared day changes, so the at-a-glance stats follow it.
  $effect(() => {
    day.offset
    load()
  })

  let pet = $derived(list.find((p) => p.id === selected) ?? null)
  let watch = $derived(pet?.wellbeing.kind === 'watch')

  // --- deep links into Moments -----------------------------------------------
  // Map a stat's label to the behaviour facet it is counted from, so a tile opens exactly
  // the moments behind its own number.
  //
  // These used to map to ACTIVITY values instead: "Meals" counts the `ate` flag but linked
  // to `activity=eating`, a different set, and "Rest" counts sleeping + resting + sitting
  // but linked to `activity=resting`, a subset. Every tile therefore opened a list that
  // disagreed with the figure printed on it. The behaviour facet filters the same column
  // the statistic sums.
  const BEH_BY_LABEL: Record<string, BehaviourValue> = {
    Meals: 'ate',
    Water: 'drank',
    Rest: 'rest',
    Rested: 'rest',
    Litter: 'eliminated',
    Play: 'played',
    Ate: 'ate',
    Groomed: 'groomed',
    Slept: 'slept',
  }
  const behFor = (label: string): BehaviourValue | undefined => BEH_BY_LABEL[label]
  // Date ranges relative to the selected day (offset): the day itself, the trailing
  // week, and the trailing 30 days. A single day passes from === to.
  const selYmd = () => ymdForOffset(day.offset)
  const weekRange = (): ReviewPreset => ({ from: ymdForOffset(day.offset + 6), to: ymdForOffset(day.offset) })
  const monthRange = (): ReviewPreset => ({ from: ymdForOffset(day.offset + 29), to: ymdForOffset(day.offset) })
  function reviewFor(petId: string, extra: ReviewPreset) {
    onnav('review', { subject: [{ kind: 'pet', petId }], ...extra })
  }

  function tileGlyph(v: string): string {
    if (v === 'yes') return '✓'
    if (v === 'lots' || v === 'some') return v
    return '–'
  }
  function tileColor(kind: string): string {
    if (kind === 'watch') return 'var(--watch)'
    if (kind === 'good') return 'var(--text)'
    return 'var(--faint)'
  }
  const max = (xs: number[]) => Math.max(1, ...xs)
  // A stat with nothing behind it opens an empty list, so it does not open, and says so by
  // looking inert. Written once: the three tile grids and the spark bar had four copies of
  // this between them, and the fourth had already lost half the treatment.
  const inert = (empty: boolean) => ({
    cls: empty ? 'cursor-default' : 'tappable',
    style: `background:${empty ? 'transparent' : 'var(--surface)'};border-color:var(--line);color:inherit;opacity:${empty ? 0.55 : 1}`,
    title: empty ? 'Nothing to show for this one' : 'See those moments',
  })

  // Fetch a moment by id and open it in the lightbox (a keepsake or last-seen only
  // carries an obsId, so the full moment is resolved on demand).
  async function openMoment(obsId: number) {
    try {
      const obs = await api.moment(obsId)
      openLightbox([obs], obs.id, pets, load)
    } catch (e) {
      error = friendlyError(e)
    }
  }
</script>

<div class="fade px-[18px] pt-[14px] pb-[120px] lg:mx-auto lg:max-w-[760px] lg:px-[32px] lg:pt-[24px] lg:pb-[60px] xl:max-w-[1180px]">
  <div class="mb-[12px] flex items-center justify-between gap-[10px]">
    <div class="font-head text-[22px] font-semibold" style="color:var(--text)">Your pets</div>
    <span class="lg:hidden"><GearButton {onnav} /></span>
  </div>

  <DayNav />

  {#if error}
    <p class="mb-3 text-[13px]" style="color:#e26d5c">{error}</p>
  {/if}

  {#if !loaded}
    <div class="flex justify-center py-[40px]" style="color:var(--accent);opacity:.5"><Paw size={30} /></div>
  {:else if list.length === 0}
    <p class="text-[13.5px]" style="color:var(--muted)">No pets yet. Add them in setup (the gear on Today).</p>
  {:else if pet}
    <!-- The pet selector: a horizontal chip strip on mobile, a vertical rail on
         desktop. Declared once and rendered into whichever layout applies. -->
    {#snippet petTabs(vertical: boolean)}
      {#if vertical}
        <div class="flex flex-col gap-[4px]">
          {#each list as p (p.id)}
            {@const on = p.id === selected}
            <button
              onclick={() => (selected = p.id)}
              class="flex w-full items-center gap-[11px] rounded-[16px] p-[8px] text-left"
              style="border:1px solid {on ? 'transparent' : 'var(--line)'};background:{on ? 'var(--surface2)' : 'transparent'};box-shadow:{on ? '0 2px 10px rgba(0,0,0,0.22)' : 'none'}"
            >
              <Avatar name={p.name} species={p.species} size={38} photo={photoUrlFor(pets, p.id)} />
              <div class="min-w-0 flex-1">
                <div class="font-head text-[15px] font-semibold" style="color:{on ? 'var(--text)' : 'var(--muted)'}">{p.name}</div>
                <div class="truncate text-[11.5px] font-bold capitalize" style="color:var(--faint)">{p.species}</div>
              </div>
            </button>
          {/each}
        </div>
      {:else}
        <div class="mb-[20px] flex gap-[9px] overflow-x-auto pb-[2px]" data-scroll>
          {#each list as p (p.id)}
            {@const on = p.id === selected}
            <button
              onclick={() => (selected = p.id)}
              class="flex flex-shrink-0 items-center gap-[9px] rounded-full py-[6px] pr-[15px] pl-[6px]"
              style="border:1px solid {on ? 'transparent' : 'var(--line)'};background:{on ? 'var(--surface2)' : 'transparent'};box-shadow:{on ? '0 2px 10px rgba(0,0,0,0.22)' : 'none'}"
            >
              <Avatar name={p.name} species={p.species} size={34} photo={photoUrlFor(pets, p.id)} />
              <span class="font-head text-[15px] font-semibold" style="color:{on ? 'var(--text)' : 'var(--muted)'}">{p.name}</span>
            </button>
          {/each}
        </div>
      {/if}
    {/snippet}

    {#snippet detail()}
      {#if pet}
      <!-- header -->
    <div class="mb-[16px] flex items-center gap-[16px]">
      <Avatar name={pet.name} species={pet.species} size={78} photo={photoUrlFor(pets, pet.id)} />
      <div class="min-w-0 flex-1">
        <div class="flex flex-wrap items-center gap-[9px]">
          <span class="font-head text-[28px] leading-none font-bold" style="color:var(--text)">{pet.name}</span>
          <span class="inline-flex items-center gap-[5px] rounded-full px-[11px] py-[4px] text-[11.5px] font-extrabold" style="background:{watch ? 'rgba(236,177,99,0.16)' : 'rgba(163,192,143,0.16)'};color:{watch ? 'var(--watch)' : 'var(--good)'}"><span class="h-[6px] w-[6px] rounded-full" style="background:{watch ? 'var(--watch)' : 'var(--good)'}"></span>{watch ? 'Worth a look' : 'All settled'}</span>
          {#if pet.anomaly}
            <span class="inline-flex items-center gap-[5px] rounded-full px-[11px] py-[4px] text-[11.5px] font-extrabold" style={chipStyle(pet.anomaly.kind)}>{pet.anomaly.label}</span>
          {/if}
        </div>
        <div class="mt-[5px] text-[13px] font-semibold capitalize" style="color:var(--muted)">
          {pet.species}
        </div>
        <div class="mt-[8px] flex flex-wrap gap-[6px]">
          {#each pet.glance as c, i (i)}
            <Chip label={c.label} kind={c.kind} size="md" />
          {/each}
        </div>
      </div>
    </div>

    {#if pet.caveat}
      <div class="mb-[16px] flex items-start gap-[9px] rounded-2xl px-[14px] py-[12px]" style="background:rgba(163,192,143,0.1);border:1px solid rgba(163,192,143,0.28)">
        <span class="text-[15px]" style="color:var(--good)">♥</span>
        <div>
          <div class="mb-[2px] text-[11.5px] font-extrabold tracking-wide uppercase" style="color:var(--good)">Good to know</div>
          <div class="text-[13.5px] leading-[1.4]" style="color:var(--text)">{pet.caveat}</div>
        </div>
      </div>
    {/if}

    <!-- last seen -->
    {#if pet.lastSeen}
      {@const ls = pet.lastSeen}
      <button onclick={() => openMoment(ls.obsId)} class="tappable mb-[18px] flex w-full items-center gap-[13px] rounded-[20px] border px-[13px] py-[11px] text-left" style="background:var(--surface);border-color:var(--line);color:inherit">
        <div class="relative h-[52px] w-[52px] flex-shrink-0 overflow-hidden rounded-[15px]" style="background:{avatarGradient(pet.species)}">
          <div class="absolute inset-0 flex items-center justify-center" style="color:rgba(255,246,236,0.24)"><Paw size={22} /></div>
        </div>
        <div class="min-w-0 flex-1">
          <div class="text-[10.5px] font-extrabold tracking-wider uppercase" style="color:var(--muted)">Last seen</div>
          <div class="font-head mt-[1px] text-[16px] font-semibold" style="color:var(--text)">{relTime(ls.at)}</div>
          <div class="mt-[1px] text-[12px]" style="color:var(--muted)">{ls.line} · {ls.room}</div>
        </div>
        <span class="flex-shrink-0 text-[20px]" style="color:var(--faint)">›</span>
      </button>
    {/if}

    <!-- day tiles -->
    <div class="mx-[2px] mb-[10px] flex items-baseline justify-between gap-[8px]">
      <span class="font-head text-[16px] font-semibold" style="color:var(--text)">{day.offset === 0 ? 'Today at a glance' : `${day.label} at a glance`}</span>
      {#if pet.tiles.some((t) => t.value !== 'no' && t.value !== 'none')}
        <span class="text-[11px] font-semibold" style="color:var(--faint)">Tap any stat to see those moments</span>
      {/if}
    </div>
    <div class="mb-[22px] grid grid-cols-2 gap-[10px] xl:grid-cols-4">
      {#each pet.tiles as t, i (i)}
        {@const empty = t.value === 'no' || t.value === 'none'}
        <!-- An empty tile is inert, and now looks it. Five identical tiles of which two
             respond to a tap is a coin toss, under a caption promising all of them do. -->
        <button
          onclick={empty ? undefined : () => reviewFor(pet.id, { from: selYmd(), to: selYmd(), behaviour: behFor(t.label) })}
          disabled={empty}
          title={inert(empty).title}
          class="{inert(empty).cls} rounded-[18px] border p-[14px] text-left"
          style={inert(empty).style}
        >
          <div class="flex items-baseline justify-between">
            <span class="text-[12.5px] font-bold" style="color:var(--muted)">{t.label}</span>
            <span class="font-head text-[18px] font-semibold" style="color:{tileColor(t.kind)}">{tileGlyph(t.value)}</span>
          </div>
          <div class="mt-[4px] text-[11.5px]" style="color:var(--faint)">{t.sub}</div>
        </button>
      {/each}
    </div>

    <!-- last 30 days (from the SQL facts projection) -->
    {#if pet.monthStats.length}
      <div class="font-head mx-[2px] mb-[10px] text-[16px] font-semibold" style="color:var(--text)">Last 30 days</div>
      <div class="mb-[22px] flex flex-wrap gap-[8px]">
        {#each pet.monthStats as s, i (i)}
          {@const empty = /^0\b/.test(s.v)}
          <button
            onclick={empty ? undefined : () => reviewFor(pet.id, { ...monthRange(), behaviour: behFor(s.k) })}
            disabled={empty}
            title={inert(empty).title}
            class="{inert(empty).cls} rounded-[16px] border px-[13px] py-[10px] text-left"
            style={inert(empty).style}
          >
            <div class="text-[11px] font-bold" style="color:var(--muted)">{s.k}</div>
            <div class="font-head text-[15px] font-semibold" style="color:var(--text)">{s.v}</div>
          </button>
        {/each}
      </div>
    {/if}

    <!-- charts: a two-column grid on desktop; stacked in the same order on mobile -->
    <div class="xl:grid xl:grid-cols-2 xl:items-start xl:gap-[16px]">
    <div class="flex min-w-0 flex-col">
    <!-- this week -->
    <div class="font-head mx-[2px] mb-[10px] text-[16px] font-semibold" style="color:var(--text)">This week</div>
    <div class="mb-[14px] rounded-[24px] border p-[18px]" style="background:var(--surface);border-color:var(--line)">
      <div class="mb-[11px] text-[11.5px] font-bold" style="color:var(--faint)">How often we saw {pet.name} each day</div>
      <div class="mb-[7px] flex h-[54px] items-end gap-[8px]">
        {#each pet.spark as v, i (i)}
          {@const isSel = i === pet.spark.length - 1}
          {@const dOff = day.offset + pet.spark.length - 1 - i}
          <!-- A bar for a day with no sightings opens an empty list, so it does not open.
               The tiles above already refuse an empty stat; this is the same rule. -->
          <button onclick={v === 0 ? undefined : () => reviewFor(pet.id, { from: ymdForOffset(dOff), to: ymdForOffset(dOff) })} disabled={v === 0} class="{v === 0 ? 'cursor-default' : 'tappable-bar'} flex h-full flex-1 items-end" style="border:none;background:none;padding:0" aria-label={v === 0 ? 'Nothing seen that day' : "See that day's moments"}>
            <div
              class="w-full rounded-t-[6px]"
              style="height:{v === 0 ? 4 : 12 + (v / max(pet.spark)) * 40}px;background:{v === 0
                ? 'var(--line)'
                : 'linear-gradient(180deg,var(--accent),rgba(217,139,111,0.55))'};opacity:{v === 0 || isSel
                ? 1
                : 0.6};box-shadow:{isSel && v > 0 ? '0 0 0 2px var(--surface),0 0 0 3px rgba(236,171,130,0.5)' : 'none'}"
            ></div>
          </button>
        {/each}
      </div>
      <div class="flex gap-[8px]">
        {#each ['M', 'T', 'W', 'T', 'F', 'S', 'S'] as d, i (i)}
          {@const isSel = i === pet.spark.length - 1}
          <span class="flex-1 text-center text-[10px] font-extrabold" style="color:{isSel ? 'var(--accent)' : 'var(--faint)'}">{isSel && day.offset === 0 ? 'Today' : d}</span>
        {/each}
      </div>
    </div>

    <!-- daily habits -->
    <div class="font-head mx-[2px] mt-[4px] mb-[3px] text-[16px] font-semibold" style="color:var(--text)">Daily habits</div>
    <div class="mx-[2px] mb-[10px] text-[12.5px]" style="color:var(--muted)">Roughly how many times a day, across the week.</div>
    <div class="mx-[2px] mb-[10px] text-[11px] leading-[1.4]" style="color:var(--faint)">These counts only reflect what the cameras can see. A pet eating or using the litter tray off-camera won't show up here.</div>
    <div class="mb-[14px] grid grid-cols-2 gap-[10px]">
      {#each pet.habits as h, i (i)}
        {@const empty = h.usual === 0 && h.values.every((v) => v === 0)}
        <button
          onclick={empty ? undefined : () => reviewFor(pet.id, { ...weekRange(), behaviour: behFor(h.label) })}
          disabled={empty}
          title={inert(empty).title}
          class="{inert(empty).cls} flex min-h-[112px] flex-col rounded-[18px] border p-[14px] text-left"
          style={inert(empty).style}
        >
          <div class="text-[12.5px] font-bold" style="color:var(--muted)">{h.label}</div>
          {#if empty}
            <div class="font-head mt-[3px] text-[13px] font-semibold leading-[1.3]" style="color:var(--faint)">Not captured by the cameras</div>
          {:else}
            <div class="font-head mt-[3px] text-[16px] font-semibold" style="color:var(--text)">{h.summary}</div>
            {#if h.belowUsual}
              <div class="mt-[3px] text-[10.5px] font-extrabold" style="color:var(--watch)">a little less lately</div>
            {/if}
            <div class="relative mt-auto flex h-[30px] w-full items-end gap-[3px] pt-[14px]">
              <div class="absolute right-0 left-0" style="bottom:{Math.round((h.usual / max([...h.values, h.usual])) * 30)}px;height:0;border-top:1.5px dashed var(--faint);opacity:.7"></div>
              {#each h.values as v, j (j)}
                <div class="min-w-0 flex-1 rounded-t-[3px]" style="height:{Math.max(3, Math.round((v / max([...h.values, h.usual])) * 30))}px;background:{v < h.usual ? 'var(--watch)' : 'linear-gradient(180deg,var(--accent),rgba(217,139,111,0.5))'};opacity:{v < h.usual ? 0.9 : 1}"></div>
              {/each}
            </div>
          {/if}
        </button>
      {/each}
    </div>
    </div>
    <div class="flex min-w-0 flex-col">

    <!-- typical day -->
    <div class="mb-[14px] rounded-[24px] border p-[18px]" style="background:var(--surface);border-color:var(--line)">
      <div class="font-head text-[16px] font-semibold" style="color:var(--text)">A typical day</div>
      <div class="mt-[2px] mb-[16px] text-[12.5px]" style="color:var(--muted)">When {pet.name} is usually up and about.</div>
      <button onclick={() => reviewFor(pet.id, weekRange())} class="tappable-bar flex h-[52px] w-full items-end gap-[2px]" style="border:none;background:none;padding:0" aria-label="See this week's moments">
        {#each pet.rhythm as v, i (i)}
          <div class="min-w-0 flex-1 rounded-t-[3px]" style="height:{v === 0 ? '10%' : 20 + (v / max(pet.rhythm)) * 80 + '%'};background:{v === 0 ? 'var(--rest)' : 'var(--accent)'};opacity:{v === 0 ? 0.5 : 1}"></div>
        {/each}
      </button>
      <div class="mt-[5px] flex gap-[2px]">
        {#each pet.rhythm as _v, i (i)}
          <span class="flex h-[6px] flex-1 justify-center">
            {#if pet.rhythmMarks.includes(i)}
              <span class="h-[5px] w-[5px] rounded-full" style="background:var(--good)"></span>
            {/if}
          </span>
        {/each}
      </div>
      <div class="mt-[4px] flex justify-between text-[9.5px] font-bold" style="color:var(--faint)">
        <span>12a</span><span>6a</span><span>12p</span><span>6p</span><span>12a</span>
      </div>
      {#if pet.rhythmMarks.length}
        <div class="mt-[12px] flex items-center gap-[6px] text-[11px] font-bold" style="color:var(--muted)">
          <span class="h-[8px] w-[8px] rounded-full" style="background:var(--good)"></span>dots mark meals &amp; drinks
        </div>
      {/if}
      <div class="mt-[16px] border-t pt-[16px]" style="border-color:var(--line)">
        <div class="mb-[9px] flex items-center justify-between text-[12.5px] font-bold" style="color:var(--text)">
          <span>Resting</span><span>Up &amp; about</span>
        </div>
        <div class="flex h-[34px] gap-[3px] overflow-hidden rounded-xl">
          <div class="flex items-center pl-[12px]" style="width:{pet.balance.restPct}%;background:var(--rest)"><span class="font-head text-[13px]" style="color:#3a2830">{pet.balance.restPct}%</span></div>
          <div class="flex items-center justify-end pr-[12px]" style="width:{100 - pet.balance.restPct}%;background:var(--accent)"><span class="font-head text-[13px]" style="color:var(--ink)">{100 - pet.balance.restPct}%</span></div>
        </div>
        <div class="mt-[13px] text-[12.5px] leading-[1.45]" style="color:var(--text);opacity:.9">{pet.balance.caption}</div>
      </div>
    </div>

    <!-- favourite spots -->
    {#if pet.spots.length}
      <div class="mb-[14px] rounded-[24px] border p-[18px]" style="background:var(--surface);border-color:var(--line)">
        <div class="font-head text-[16px] font-semibold" style="color:var(--text)">Favourite spots</div>
        <div class="mt-[2px] mb-[16px] text-[12.5px]" style="color:var(--muted)">Where {pet.name} likes to be.</div>
        <div class="flex flex-col gap-[12px]">
          {#each pet.spots as sp, i (i)}
            <button onclick={() => reviewFor(pet.id, { ...weekRange(), camera: sp.cameras })} class="flex w-full items-center gap-[12px]" style="border:none;background:none;padding:0;color:inherit">
              <span class="w-[120px] flex-shrink-0 truncate text-left text-[13px] font-bold" style="color:var(--text)">{sp.room}</span>
              <div class="h-[8px] flex-1 overflow-hidden rounded-full" style="background:var(--surface2)">
                <div class="h-full rounded-full" style="width:{sp.pct}%;background:{i === 0 ? 'var(--accent)' : 'rgba(236,171,130,0.4)'}"></div>
              </div>
              <span class="w-[34px] flex-shrink-0 text-right text-[12px] font-bold" style="color:var(--muted)">{sp.pct}%</span>
            </button>
          {/each}
        </div>
      </div>
    {/if}
    </div>
    </div>

    <!-- wellbeing: shown only when there is a real signal (a watch, or written
         text), so the "everything is fine" case adds no noise. -->
    {#if pet.wellbeing.kind === 'watch' || pet.wellbeing.text}
      <div class="mb-[14px] rounded-[20px] p-[16px]" style="background:{pet.wellbeing.kind === 'watch' ? 'rgba(236,177,99,0.12)' : 'rgba(163,192,143,0.1)'};border:1px solid {pet.wellbeing.kind === 'watch' ? 'rgba(236,177,99,0.3)' : 'rgba(163,192,143,0.28)'}">
        <div class="mb-[7px] flex items-center gap-[8px]">
          <span class="h-[9px] w-[9px] rounded-full" style="background:{pet.wellbeing.kind === 'watch' ? 'var(--watch)' : 'var(--good)'}"></span>
          <span class="text-[11.5px] font-extrabold tracking-wide uppercase" style="color:{pet.wellbeing.kind === 'watch' ? 'var(--watch)' : 'var(--good)'}">Wellbeing</span>
        </div>
        <div class="text-[13.5px] leading-[1.55]" style="color:var(--text);opacity:.92">
          {pet.wellbeing.text ?? 'One gentle thing to keep an eye on this week.'}
        </div>
      </div>
    {/if}

    <!-- keepsake -->
    <div class="mx-[2px] mt-[6px] mb-[10px] flex items-center justify-between">
      <span class="font-head text-[16px] font-semibold" style="color:var(--text)">Moments worth keeping</span>
      <button onclick={() => onnav('keepsakes')} class="bg-transparent text-[12.5px] font-bold" style="border:none;color:var(--accent)">See all ›</button>
    </div>
    {#if pet.keepsake}
      {@const k = pet.keepsake}
      <button onclick={() => openMoment(k.obsId)} class="tappable w-full rounded-[24px] border p-[10px] text-left" style="background:var(--surface);border-color:var(--line);color:inherit">
        <div class="relative mb-[11px] w-full overflow-hidden rounded-[18px]" style="aspect-ratio:16/10">
          <Thumb img={k.img} media={k.media} room={k.room} pawSize={52} />
          <span class="absolute bottom-[9px] left-[11px] rounded-[6px] px-[6px] py-[2px] font-mono text-[9px]" style="color:rgba(255,246,236,0.78);background:rgba(20,14,10,0.42)">{k.room}</span>
        </div>
        <div class="flex items-center gap-[8px] px-[4px] pb-[4px]">
          <span class="text-[15px]" style="color:var(--accent)">♥</span>
          <span class="font-head text-[15px] font-medium" style="color:var(--text)">{k.caption ?? 'A moment worth keeping'}</span>
        </div>
      </button>
    {:else}
      <div class="rounded-[20px] border border-dashed p-[16px] text-center text-[12.5px] leading-[1.5]" style="background:var(--surface);border-color:var(--line);color:var(--muted)">Nothing kept for {pet.name} yet. Open a moment and tap Keep.</div>
    {/if}

    <!-- weekly recap -->
    {#if pet.recap}
      <div class="font-head mx-[2px] mt-[22px] mb-[10px] text-[16px] font-semibold" style="color:var(--text)">Your week with {pet.name}</div>
      <div class="relative overflow-hidden rounded-[26px] border p-[22px]" style="background:linear-gradient(155deg,var(--surface2),var(--surface));border-color:var(--line);box-shadow:0 16px 40px -18px rgba(0,0,0,0.6)">
        <div class="absolute -top-[20px] -right-[18px]" style="color:var(--accent);opacity:.1"><Paw size={130} /></div>
        <div class="relative mb-[16px] flex items-center gap-[13px]">
          <Avatar name={pet.name} species={pet.species} size={52} photo={photoUrlFor(pets, pet.id)} />
          <div class="min-w-0 flex-1">
            <div class="font-head text-[19px] leading-[1.05] font-semibold" style="color:var(--text)">{pet.name}'s week</div>
            <div class="mt-[2px] text-[12px] font-bold" style="color:var(--muted)">{pet.recap.range}</div>
          </div>
          <span class="flex-shrink-0 text-[17px]" style="color:var(--accent)">🐾</span>
        </div>
        <div class="relative mb-[16px] grid grid-cols-2 gap-[9px]">
          {#each pet.recap.stats as s, i (i)}
            <div class="rounded-[15px] border px-[13px] py-[11px]" style="background:rgba(255,246,236,0.05);border-color:var(--line)">
              <div class="text-[10.5px] font-extrabold tracking-wide uppercase" style="color:var(--muted)">{s.k}</div>
              <div class="font-head mt-[3px] text-[15.5px] font-semibold" style="color:var(--text)">{s.v}</div>
            </div>
          {/each}
        </div>
      </div>
    {/if}
      {/if}
    {/snippet}

    {#if layout.isWide}
      <!-- desktop: a slim pet rail beside the profile detail -->
      <div class="grid grid-cols-[240px_1fr] items-start gap-[26px]">
        <div>{@render petTabs(true)}</div>
        <div class="min-w-0">{@render detail()}</div>
      </div>
    {:else}
      {@render petTabs(false)}
      {@render detail()}
    {/if}
  {/if}
</div>
