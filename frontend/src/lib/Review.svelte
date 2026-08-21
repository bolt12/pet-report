<script lang="ts">
  import {
    api,
    petPhotoUrl,
    subjectParam,
    type ActivityValue,
    type CorrectReq,
    type BehaviourValue,
    type MediaValue,
    type MomentsQuery,
    type ObsView,
    type Profile,
    type ReviewPreset,
    type SubjectSel,
    type TimeOfDayValue,
    type WellbeingValue,
  } from './api'
  import Thumb from './Thumb.svelte'
  import Avatar from './Avatar.svelte'
  import Paw from './Paw.svelte'
  import DayNav from './DayNav.svelte'
  import GearButton from './GearButton.svelte'
  import WellbeingDot from './WellbeingDot.svelte'
  import { openLightbox } from './lightbox.svelte'
  import { day } from './day.svelte'
  import { prefs, toggleMomentOrder } from './prefs.svelte'
  import { layout } from './layout.svelte'
  import { fmtTime, fmtDate, ymdForOffset, offsetForIso, friendlyError, serverMessage, chipStyle } from './ui'

  let { profile, preset = {}, onnav }: { profile: Profile; preset?: ReviewPreset; onnav: (s: string) => void } = $props()

  type Review = 'all' | 'reviewed' | 'unreviewed' | 'needs-look'
  // A one-time plain snapshot of the incoming preset (read inside a function so it is a
  // capture, not a live reference), so the seeds below don't track later prop changes.
  function readPreset(): ReviewPreset {
    return $state.snapshot(preset) as ReviewPreset
  }
  const p0 = readPreset()

  // Single-select facets (the server browse takes one value per facet). Seeded straight
  // from the preset: it is already a query, so there is no tag-to-facet translation left
  // to get wrong.
  // Multi-select, AND-ed by the server: a moment must hold every subject picked. One
  // chip per pet plus a person and a not-my-pet chip, so "Mochi and a person" is a
  // question the filter can ask.
  let fSubjects = $state<SubjectSel[]>(p0.subject ?? [])
  let fAct = $state<ActivityValue | null>(p0.activity ?? null)
  let fBeh = $state<BehaviourValue | null>(p0.behaviour ?? null)
  let fWell = $state<WellbeingValue | null>(p0.wellbeing ?? null)
  let fRoom = $state<string | null>(p0.room ?? null)
  let fCams = $state<string[]>(p0.camera ?? [])
  let fMedia = $state<MediaValue | null>(p0.media ?? null)
  let fTime = $state<TimeOfDayValue | null>(p0.timeOfDay ?? null)
  let fReview = $state<Review>(p0.review ?? 'all')
  let searchInput = $state('')
  let search = $state('')
  let filterOpen = $state(false)

  // Range mode (from/to) vs day mode (following the shared day store). A deep-link with
  // from/to pins that range; everything else, a needs-a-look nudge included, follows the
  // shared day, so the nudge opens the day it was tapped on rather than a global backlog.
  // The full backlog is still reachable here by pairing the needs-look facet with a range.
  // A preset naming ONE day is a day, not a range: it moves the shared day navigator and
  // leaves the screen in day mode. Every in-app nudge builds its query from the day already
  // in view, so treating those as ranges replaced the navigator with a "Date range · Aug 18"
  // card and a "Back to daily" button that went precisely nowhere.
  const oneDay = p0.from && p0.to && p0.from === p0.to ? p0.from : null
  if (oneDay) day.offset = offsetForIso(oneDay)
  const initRange = !oneDay && p0.from && p0.to ? { from: p0.from, to: p0.to } : null
  let range = $state<{ from: string; to: string } | null>(initRange)
  let fromDate = $state(initRange?.from ?? '')
  let toDate = $state(initRange?.to ?? '')
  let rangeLabel = $derived(
    range ? (range.from === range.to ? fmtDate(range.from) : `${fmtDate(range.from)} to ${fmtDate(range.to)}`) : '',
  )

  // Page state, populated by the server-side faceted browse.
  let items = $state<ObsView[]>([])
  let nextCursor = $state<string | undefined>(undefined)
  let total = $state<number | undefined>(undefined)
  let loading = $state(true)
  let loadingMore = $state(false)
  let error = $state('')
  let reviewedIds = $state<Set<number>>(new Set())
  let reassignId = $state<number | null>(null)
  let confirmDelId = $state<number | null>(null)

  // Debounce the search box so a keystroke does not fire a request each time.
  $effect(() => {
    const v = searchInput
    const t = setTimeout(() => (search = v.trim()), 300)
    return () => clearTimeout(t)
  })

  function buildQuery(cursor?: string): MomentsQuery {
    return {
      from: range?.from ?? day.iso,
      to: range?.to ?? day.iso,
      subject: fSubjects.length ? fSubjects : undefined,
      activity: fAct ?? undefined,
      behaviour: fBeh ?? undefined,
      wellbeing: fWell ?? undefined,
      room: fRoom ?? undefined,
      camera: fCams.length ? fCams : undefined,
      media: fMedia ?? undefined,
      timeOfDay: fTime ?? undefined,
      review: fReview === 'all' ? undefined : fReview,
      search: search || undefined,
      sort: prefs.momentOrder === 'newest' ? 'desc' : 'asc',
      cursor,
      limit: 50,
    }
  }

  // A monotonic request id, so a slow response for a facet the owner has changed away
  // from is dropped rather than overwriting the current view.
  let reqId = 0
  async function loadPage(reset: boolean) {
    const id = ++reqId
    if (reset) loading = true
    else loadingMore = true
    error = ''
    try {
      const page = await api.moments(buildQuery(reset ? undefined : nextCursor))
      if (id !== reqId) return
      items = reset ? page.items : [...items, ...page.items]
      nextCursor = page.nextCursor
      if (reset) total = page.total
    } catch (e) {
      if (id === reqId) error = friendlyError(e)
    } finally {
      if (id === reqId) {
        loading = false
        loadingMore = false
      }
    }
  }
  const reload = () => loadPage(true)

  // Reload from the first page whenever a facet, the range, the day (in day mode), the
  // sort, or the (debounced) search changes.
  $effect(() => {
    range
    if (!range) day.offset
    fSubjects
    fAct
    fBeh
    fWell
    fRoom
    fCams
    fMedia
    fTime
    fReview
    search
    prefs.momentOrder
    reviewedIds = new Set()
    loadPage(true)
  })

  function applyRange() {
    if (!fromDate || !toDate) return
    range = { from: fromDate, to: toDate }
    filterOpen = false
  }
  function allDates() {
    fromDate = ymdForOffset(day.earliestOffset)
    toDate = ymdForOffset(0)
    range = { from: fromDate, to: toDate }
    filterOpen = false
  }
  function exitRange() {
    range = null
  }

  // Facet options. Rooms come from the profile's camera map (no full-set fetch needed);
  // activities are the meaningful subset of what the model reports.
  const ACTIVITIES: { key: ActivityValue; label: string }[] = [
    { key: 'eating', label: 'Eating' },
    { key: 'drinking', label: 'Drinking' },
    { key: 'sleeping', label: 'Sleeping' },
    { key: 'resting', label: 'Resting' },
    { key: 'playing', label: 'Playing' },
    { key: 'grooming', label: 'Grooming' },
    { key: 'eliminating', label: 'Litter' },
    { key: 'walking', label: 'Walking' },
    { key: 'alert', label: 'Alert' },
  ]
  // The behaviour facet, matching the columns the per-pet tiles count. A tile links on
  // the same word it counted, so its number and its list are the same set.
  const BEHAVIOURS: { key: BehaviourValue; label: string }[] = [
    { key: 'ate', label: 'Ate' },
    { key: 'drank', label: 'Drank' },
    { key: 'slept', label: 'Slept' },
    { key: 'played', label: 'Played' },
    { key: 'groomed', label: 'Groomed' },
    { key: 'eliminated', label: 'Litter' },
    { key: 'rest', label: 'Resting' },
    { key: 'active', label: 'Active' },
    { key: 'concern', label: 'Health signal' },
  ]
  const MEDIA: { key: MediaValue; label: string }[] = [
    { key: 'photo', label: 'Photo' },
    { key: 'clip', label: 'Clip' },
    { key: 'audio', label: 'Audio' },
  ]
  const TIMES: { key: TimeOfDayValue; label: string }[] = [
    { key: 'morning', label: 'Morning' },
    { key: 'afternoon', label: 'Afternoon' },
    { key: 'evening', label: 'Evening' },
    { key: 'night', label: 'Night' },
  ]
  let rooms = $derived([...new Set(profile.cameras.map((c) => c.room))])
  // Species the loaded page holds that are nobody's pet: a neighbour's rabbit, a fox at the
  // door. Without these the Who row could not ask for a moment whose own card says "Rabbit",
  // so it was the one subject on screen no filter could reach.
  let otherSpecies = $derived(
    [
      ...new Set(
        items
          .flatMap((o) => o.subjects)
          .filter((s) => !s.person && !s.petId && s.species && !profile.pets.some((p) => p.petSpecies === s.species))
          .map((s) => s.species as string),
      ),
    ].sort(),
  )
  const petName = (id: string) => profile.pets.find((p) => p.petId === id)?.petName ?? id
  // Subjects are compared by their wire rendering, which is the one place a selector's
  // identity is already defined.
  const sameSubject = (a: SubjectSel, b: SubjectSel) => subjectParam(a) === subjectParam(b)
  const hasSubject = (s: SubjectSel) => fSubjects.some((x) => sameSubject(x, s))
  function toggleSubject(s: SubjectSel) {
    fSubjects = hasSubject(s) ? fSubjects.filter((x) => !sameSubject(x, s)) : [...fSubjects, s]
  }
  const subjectLabel = (s: SubjectSel) =>
    s.kind === 'pet'
      ? petName(s.petId)
      : s.kind === 'species'
        ? s.species
        : s.kind === 'person'
          ? 'A person'
          : 'Not my pet'
  const behLabel = (k: BehaviourValue) => BEHAVIOURS.find((b) => b.key === k)?.label ?? k
  const actLabel = (k: ActivityValue) => ACTIVITIES.find((a) => a.key === k)?.label ?? k
  const mediaLabel = (k: MediaValue) => MEDIA.find((m) => m.key === k)?.label ?? k
  const timeLabel = (k: TimeOfDayValue) => TIMES.find((t) => t.key === k)?.label ?? k
  const reviewLabel = (r: Review) =>
    r === 'reviewed' ? 'Reviewed' : r === 'unreviewed' ? 'Not reviewed' : r === 'needs-look' ? 'Needs a look' : ''

  let filterCount = $derived(
    fSubjects.length +
      (fAct ? 1 : 0) +
      (fBeh ? 1 : 0) +
      (fWell ? 1 : 0) +
      (fRoom ? 1 : 0) +
      (fCams.length ? 1 : 0) +
      (fMedia ? 1 : 0) +
      (fTime ? 1 : 0) +
      (fReview !== 'all' ? 1 : 0),
  )
  function clearAll() {
    fSubjects = []
    fAct = null
    fBeh = null
    fWell = null
    fRoom = null
    fCams = []
    fMedia = null
    fTime = null
    fReview = 'all'
    searchInput = ''
    search = ''
  }
  // Tri-state review chip: tapping the active one clears back to "all".
  const setReview = (v: Review) => (fReview = fReview === v ? 'all' : v)

  let activeChips = $derived([
    ...(fReview !== 'all' ? [{ label: reviewLabel(fReview), remove: () => (fReview = 'all') }] : []),
    ...fSubjects.map((s) => ({ label: subjectLabel(s), remove: () => toggleSubject(s) })),
    ...(fAct ? [{ label: actLabel(fAct), remove: () => (fAct = null) }] : []),
    ...(fBeh ? [{ label: behLabel(fBeh), remove: () => (fBeh = null) }] : []),
    ...(fWell ? [{ label: 'Concerning', remove: () => (fWell = null) }] : []),
    ...(fRoom ? [{ label: fRoom, remove: () => (fRoom = null) }] : []),
    ...(fCams.length ? [{ label: 'Selected cameras', remove: () => (fCams = []) }] : []),
    ...(fMedia ? [{ label: mediaLabel(fMedia), remove: () => (fMedia = null) }] : []),
    ...(fTime ? [{ label: timeLabel(fTime), remove: () => (fTime = null) }] : []),
  ])

  let ordered = $derived(items)
  let shownNeeds = $derived(items.filter((o) => o.needsReview && !reviewedIds.has(o.id)))
  let needsCount = $derived(shownNeeds.length)

  function mark(id: number) {
    reviewedIds = new Set(reviewedIds).add(id)
  }

  // Every card action goes through here, for the two things none of them had: a report when
  // the server says no, and a guard against firing twice. Without it a failure was an
  // unhandled rejection and the tap simply did nothing, which is what a moment with no
  // sightings did to "Not that one" every single time.
  let acting = $state(false)
  let actionError = $state('')
  async function act(fn: () => Promise<unknown>) {
    if (acting) return
    acting = true
    actionError = ''
    try {
      await fn()
    } catch (e) {
      // The server's own words, when it has any. Every owner-facing 400 this app writes
      // ("This moment already holds 16 subjects...", "invalid cursor") was being replaced
      // here with "The server hit a problem (400)", which tells nobody anything.
      actionError = serverMessage(e)
    } finally {
      acting = false
    }
  }

  const thatsRight = (o: ObsView) =>
    act(async () => {
      await api.review([o.id])
      mark(o.id)
    })
  const reviewAllShown = () =>
    act(async () => {
      const ids = shownNeeds.map((o) => o.id)
      if (!ids.length) return
      await api.review(ids)
      reviewedIds = new Set([...reviewedIds, ...ids])
    })
  // Which sighting the inline reassign panel is naming. Keyed by moment, so opening the
  // panel on one card does not carry a selection over from another.
  let reassignIx = $state(0)
  const openReassign = (o: ObsView) => {
    reassignId = o.id
    reassignIx = o.subjects.find((s) => !s.person)?.ix ?? o.subjects[0]?.ix ?? 0
  }
  // Naming a subject changes the reading; it is not the owner saying the whole moment is
  // right, so the card keeps its "That's right" and simply shows the new name. Only the one
  // moment is re-read, so the list does not jump under a finger that is still working.
  const reassign = (o: ObsView, target: CorrectReq) =>
    act(async () => {
      await api.correct(o.id, reassignIx, target)
      const updated = await api.moment(o.id)
      items = items.map((x) => (x.id === o.id ? updated : x))
      reassignId = null
    })
  const del = (o: ObsView) =>
    act(async () => {
      await api.del(o.id)
      confirmDelId = null
      reload()
    })
  // Take back the review, keeping any correction that came with it. The wider undo, back to
  // the model's own reading, lives in the moment viewer where the corrections are visible.
  const unreview = (o: ObsView) =>
    act(async () => {
      await api.unreview(o.id)
      reviewedIds = new Set([...reviewedIds].filter((x) => x !== o.id))
    })
  function open(o: ObsView) {
    openLightbox(ordered, o.id, profile.pets, reload)
  }

  const optStyle = (active: boolean) =>
    active ? 'background:var(--accent);color:var(--ink)' : 'background:var(--surface2);color:var(--muted)'
</script>

<!-- Reusable blocks, rendered into the mobile column or the desktop
     filter-rail / results two-column grid. -->
{#snippet summaryBanner()}
  <div
    class="mb-[14px] flex items-center gap-[11px] rounded-[18px] border p-[13px]"
    style="background:{needsCount > 0 ? 'rgba(236,177,99,0.1)' : 'rgba(163,192,143,0.1)'};border-color:{needsCount > 0 ? 'rgba(236,177,99,0.3)' : 'rgba(163,192,143,0.28)'}"
  >
    <span class="flex h-[34px] w-[34px] flex-shrink-0 items-center justify-center rounded-xl" style="background:{needsCount > 0 ? 'var(--watch)' : 'var(--good)'};color:{needsCount > 0 ? '#3a2a10' : '#1f3018'}">
      {#if needsCount > 0}<Paw size={19} />{:else}<span class="text-[16px] font-black">✓</span>{/if}
    </span>
    <div class="min-w-0 flex-1">
      <div class="font-head text-[15px] font-semibold" style="color:var(--text)">{needsCount > 0 ? `${needsCount} moment${needsCount === 1 ? '' : 's'} to look at` : 'All caught up'}</div>
      <div class="mt-[1px] text-[11.5px]" style="color:var(--muted)">I only flag what I'm not sure about, everything else is fine.</div>
    </div>
  </div>
{/snippet}

{#snippet markAll()}
  {#if shownNeeds.length > 1}
    <button onclick={reviewAllShown} disabled={acting} class="mb-[12px] w-full rounded-[14px] py-[11px] text-[13px] font-extrabold disabled:opacity-50" style="background:rgba(163,192,143,0.16);color:var(--good);border:none">Mark all {shownNeeds.length} shown as right</button>
  {/if}
{/snippet}

{#snippet searchBox()}
  <input bind:value={searchInput} placeholder="Search moments..." class="mb-[12px] w-full rounded-full border px-[18px] py-[12px] text-[14px] outline-none" style="background:var(--surface);border-color:var(--line);color:var(--text)" />
{/snippet}

{#snippet sortToggle()}
  <button onclick={toggleMomentOrder} class="inline-flex flex-shrink-0 items-center gap-[5px] rounded-full border px-[13px] py-[9px] text-[13px] font-extrabold" style="background:var(--surface);border-color:var(--line);color:var(--text)" aria-label="Change sort order" title="Change sort order">
    <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round">
      {#if prefs.momentOrder === 'newest'}<path d="M12 5v14M12 19l5-5M12 19l-5-5" />{:else}<path d="M12 19V5M12 5l5 5M12 5l-5 5" />{/if}
    </svg>
    {prefs.momentOrder === 'newest' ? 'Newest' : 'Oldest'}
  </button>
{/snippet}

{#snippet activeChipsRow()}
  <div class="flex flex-wrap gap-[7px]">
    {#each activeChips as c, i (i)}
      <span class="inline-flex items-center gap-[6px] rounded-full py-[5px] pr-[8px] pl-[12px] text-[12px] font-bold" style="background:rgba(236,171,130,0.16);color:var(--accent)">{c.label}<button onclick={c.remove} class="inline-flex h-[16px] w-[16px] items-center justify-center rounded-full text-[10px] leading-none" style="border:none;background:rgba(236,171,130,0.25);color:var(--accent)">✕</button></span>
    {/each}
  </div>
{/snippet}

{#snippet filterGroups()}
  <div class="mb-[15px]">
    <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Show only</div>
    <div class="flex flex-wrap gap-[7px]">
      <button onclick={() => setReview('needs-look')} class="inline-flex items-center gap-[6px] rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fReview === 'needs-look')}><span class="h-[7px] w-[7px] rounded-full" style="background:var(--watch)"></span>Needs a look</button>
      <button onclick={() => setReview('reviewed')} class="inline-flex items-center gap-[6px] rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fReview === 'reviewed')}>Reviewed</button>
      <button onclick={() => setReview('unreviewed')} class="inline-flex items-center gap-[6px] rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fReview === 'unreviewed')}>Not reviewed</button>
    </div>
  </div>
  {#if profile.pets.length}
    <div class="mb-[15px]">
      <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Who</div>
      <div class="flex flex-wrap gap-[7px]">
        {#each profile.pets as p (p.petId)}
          <button onclick={() => toggleSubject({ kind: 'pet', petId: p.petId })} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(hasSubject({ kind: 'pet', petId: p.petId }))}>{p.petName}</button>
        {/each}
        <!-- The two subjects the facet vocabulary could not express before, so the day
             summary had to fake one of them as a pet id. -->
        <button onclick={() => toggleSubject({ kind: 'person' })} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(hasSubject({ kind: 'person' }))}>A person</button>
        <button onclick={() => toggleSubject({ kind: 'visiting' })} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(hasSubject({ kind: 'visiting' }))}>Not my pet</button>
        {#each otherSpecies as sp (sp)}
          <button onclick={() => toggleSubject({ kind: 'species', species: sp })} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold capitalize" style={optStyle(hasSubject({ kind: 'species', species: sp }))}>{sp}</button>
        {/each}
      </div>
    </div>
  {/if}
  <div class="mb-[15px]">
    <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Activity</div>
    <div class="flex flex-wrap gap-[7px]">
      {#each ACTIVITIES as a (a.key)}
        <button onclick={() => (fAct = fAct === a.key ? null : a.key)} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fAct === a.key)}>{a.label}</button>
      {/each}
    </div>
  </div>
  {#if rooms.length}
    <div class="mb-[15px]">
      <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Where</div>
      <div class="flex flex-wrap gap-[7px]">
        {#each rooms as r (r)}
          <button onclick={() => (fRoom = fRoom === r ? null : r)} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fRoom === r)}>{r}</button>
        {/each}
      </div>
    </div>
  {/if}
  <div class="mb-[15px]">
    <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Media</div>
    <div class="flex flex-wrap gap-[7px]">
      {#each MEDIA as m (m.key)}
        <button onclick={() => (fMedia = fMedia === m.key ? null : m.key)} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fMedia === m.key)}>{m.label}</button>
      {/each}
    </div>
  </div>
  <div class="mb-[15px]">
    <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Time of day</div>
    <div class="flex flex-wrap gap-[7px]">
      {#each TIMES as t (t.key)}
        <button onclick={() => (fTime = fTime === t.key ? null : t.key)} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(fTime === t.key)}>{t.label}</button>
      {/each}
    </div>
  </div>
  <div class="mb-[15px]">
    <div class="mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Dates</div>
    <div class="flex items-center gap-[8px]">
      <input type="date" bind:value={fromDate} class="min-w-0 flex-1 rounded-[11px] border px-[10px] py-[8px] text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
      <span class="text-[12px] font-bold" style="color:var(--faint)">to</span>
      <input type="date" bind:value={toDate} class="min-w-0 flex-1 rounded-[11px] border px-[10px] py-[8px] text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
    </div>
    <div class="mt-[8px] flex flex-wrap gap-[7px]">
      <button onclick={applyRange} disabled={!fromDate || !toDate} class="rounded-full px-[13px] py-[6px] text-[11.5px] font-extrabold disabled:opacity-40" style="border:none;background:var(--surface2);color:var(--accent)">Apply range</button>
      <button onclick={allDates} class="rounded-full px-[13px] py-[6px] text-[11.5px] font-bold" style="border:none;background:var(--surface2);color:var(--muted)">All dates</button>
      {#if range}<button onclick={exitRange} class="rounded-full px-[13px] py-[6px] text-[11.5px] font-bold" style="border:none;background:var(--surface2);color:var(--muted)">Back to daily</button>{/if}
    </div>
  </div>
{/snippet}

{#snippet resultsBody()}
  {#if error}<p class="mb-3 text-[13px]" style="color:#e26d5c">{error}</p>{/if}
  <!-- A failed card action. Separate from the load error above, which describes the list
       rather than the tap, and dismissible because the card it refers to is still there. -->
  {#if actionError}
    <div class="mb-3 flex items-start gap-[10px] rounded-[14px] px-[13px] py-[10px]" style="background:rgba(226,109,92,0.12);border:1px solid rgba(226,109,92,0.3)">
      <span class="min-w-0 flex-1 text-[12.5px] leading-[1.45]" style="color:#e26d5c">{actionError}</span>
      <button onclick={() => (actionError = '')} class="flex-shrink-0 text-[12px] font-bold" style="border:none;background:none;color:#e26d5c">Dismiss</button>
    </div>
  {/if}

  <!-- list -->
  {#if loading}
    <div class="flex justify-center py-[40px]" style="color:var(--accent);opacity:.5"><Paw size={30} /></div>
  {:else if items.length === 0}
    <div class="rounded-[22px] border border-dashed p-[34px] text-center" style="background:var(--surface);border-color:var(--line)">
      <div class="mb-[10px] flex justify-center" style="color:var(--accent);opacity:.5"><Paw size={36} /></div>
      <div class="mx-auto max-w-[240px] text-[13.5px] leading-[1.5]" style="color:var(--muted)">Nothing matches here, try another filter or search.</div>
    </div>
  {:else}
    <div class="flex flex-col gap-[11px] xl:grid xl:grid-cols-[repeat(auto-fill,minmax(272px,1fr))] xl:items-start">
      {#each ordered as o (o.id)}
        {@const showActions = o.needsReview && !reviewedIds.has(o.id)}
        <div class="tappable fade rounded-[22px] border p-[11px]" style="background:var(--surface);border-color:var(--line)">
          <button onclick={() => open(o)} class="flex w-full gap-[13px] text-left" style="background:none;border:none;padding:0;color:inherit">
            <div class="relative h-[74px] w-[74px] flex-shrink-0 overflow-hidden rounded-[14px]">
              <Thumb img={o.media.stillUrl} media={o.media.kind} room={o.room} pawSize={28} />
              <span class="absolute bottom-[5px] left-[6px] rounded-[6px] px-[6px] py-[2px] font-mono capitalize" style="font-size:8px;color:rgba(255,246,236,0.78);background:rgba(20,14,10,0.5)">{o.media.kind}</span>
              {#if o.kept}
                <span class="absolute top-[5px] right-[6px] rounded-full px-[4px] py-[1px]" style="font-size:9px;color:var(--good);background:rgba(20,14,10,0.5)" title="Kept">♥</span>
              {/if}
            </div>
            <div class="flex min-w-0 flex-1 flex-col gap-[4px]">
              <div class="flex items-center gap-[6px]"><WellbeingDot wellbeing={o.wellbeing} /><span class="text-[11.5px] font-extrabold" style="color:var(--text)">{fmtTime(o.at)}</span><span style="color:var(--faint)">·</span><span class="text-[11.5px] font-semibold" style="color:var(--muted)">{o.room}</span></div>
              <div class="flex flex-wrap items-center gap-[7px]"><span class="font-head text-[15px] font-semibold" style="color:var(--text)">{o.subjectLabel}</span>{#if o.uncertain}<span class="rounded-full px-[8px] py-[2px] text-[10px] font-bold whitespace-nowrap" style="color:var(--unclear);background:rgba(183,168,192,0.14)">not fully sure</span>{/if}{#if o.subjects.some((s) => s.byModel)}<span class="rounded-full px-[8px] py-[2px] text-[10px] font-bold whitespace-nowrap" style={chipStyle('info')} title="I picked this pet myself. Open it to confirm or change.">my guess</span>{/if}{#if o.reviewed && !reviewedIds.has(o.id)}<span class="rounded-full px-[8px] py-[2px] text-[10px] font-bold whitespace-nowrap" style="color:var(--good);background:rgba(163,192,143,0.16)">✓ reviewed</span>{/if}</div>
              {#if o.description}<div class="clamp2 text-[12px] leading-[1.35]" style="color:var(--muted)">{o.description}</div>{/if}
            </div>
          </button>
          {#if showActions}
            {#if reassignId === o.id}
              <div class="fade mt-[11px] border-t pt-[11px]" style="border-color:var(--line)">
                <div class="mb-[9px] flex items-center justify-between gap-[10px]">
                  <span class="text-[11.5px] font-extrabold" style="color:var(--text)">Who was it, really?</span>
                  <button onclick={() => (reassignId = null)} class="bg-transparent text-[12px] font-bold" style="border:none;color:var(--faint)">Cancel</button>
                </div>
                {#if o.subjects.length > 1}
                  <!-- Pick the subject first: this frame holds more than one, and each
                       carries its own identity. -->
                  <div class="mb-[9px] flex flex-wrap gap-[6px]">
                    {#each o.subjects as s (s.ix)}
                      <button onclick={() => (reassignIx = s.ix)} class="rounded-full px-[10px] py-[4px] text-[11.5px] font-bold" style={optStyle(reassignIx === s.ix)}>{s.label}</button>
                    {/each}
                  </div>
                {/if}
                <div class="flex flex-col gap-[6px]">
                  {#each profile.pets as p (p.petId)}
                    <button onclick={() => reassign(o, { kind: 'pet', petId: p.petId })} disabled={acting} class="flex w-full items-center gap-[9px] rounded-xl border p-[7px_9px] text-left" style="background:var(--surface2);border-color:var(--line);color:inherit">
                      <Avatar name={p.petName} species={p.petSpecies} size={30} photo={petPhotoUrl(p.petId, p.petPhoto)} />
                      <span class="font-head text-[13.5px] font-semibold" style="color:var(--text)">{p.petName}</span>
                      <span class="text-[11px] capitalize" style="color:var(--muted)">{p.petSpecies}</span>
                    </button>
                  {/each}
                  <button onclick={() => reassign(o, { kind: 'visiting' })} disabled={acting} class="flex w-full items-center gap-[9px] rounded-xl border p-[7px_9px] text-left" style="background:var(--surface2);border-color:var(--line);color:inherit">
                    <span class="flex h-[30px] w-[30px] items-center justify-center rounded-full font-head text-[13px] font-semibold" style="background:var(--surface);border:1px solid var(--line);color:var(--muted)">?</span>
                    <div class="min-w-0"><div class="font-head text-[13.5px] font-semibold" style="color:var(--text)">Not my pet</div><div class="text-[10.5px]" style="color:var(--faint)">An animal, but not one of mine</div></div>
                  </button>
                  <button onclick={() => reassign(o, { kind: 'person' })} disabled={acting} class="flex w-full items-center gap-[9px] rounded-xl border p-[7px_9px] text-left" style="background:var(--surface2);border-color:var(--line);color:inherit">
                    <span class="flex h-[30px] w-[30px] items-center justify-center rounded-full text-[15px]" style="background:var(--surface);border:1px solid var(--line);color:var(--muted)">☺</span>
                    <div class="min-w-0"><div class="font-head text-[13.5px] font-semibold" style="color:var(--text)">A person</div><div class="text-[10.5px]" style="color:var(--faint)">A human, not an animal</div></div>
                  </button>
                </div>
              </div>
            {:else}
              {#if confirmDelId === o.id}
                <div class="mt-[10px] text-[11.5px] leading-[1.4]" style="color:#e26d5c">Deletes this moment for good, plus any keepsake of it and its saved image.</div>
              {/if}
              <div class="mt-[11px] flex gap-[7px] border-t pt-[11px]" style="border-color:var(--line)">
                <button onclick={() => thatsRight(o)} disabled={acting} class="flex-1 rounded-xl py-[8px] text-[12px] font-extrabold disabled:opacity-50" style="border:none;background:rgba(163,192,143,0.16);color:var(--good)">That's right</button>
                <!-- Only where there is a subject to rename. A sound has none, and neither
                     has a frame the model read as empty, so the panel this opened could
                     only ever address a sighting that is not there. -->
                {#if o.subjects.length}
                  <button onclick={() => openReassign(o)} disabled={acting} class="flex-1 rounded-xl py-[8px] text-[12px] font-bold disabled:opacity-50" style="border:none;background:var(--surface2);color:var(--text)">Not that one</button>
                {/if}
                {#if confirmDelId === o.id}
                  <button onclick={() => del(o)} disabled={acting} class="flex-shrink-0 rounded-xl px-[12px] py-[8px] text-[12px] font-extrabold disabled:opacity-50" style="border:none;background:rgba(226,109,92,0.18);color:#e26d5c">Sure?</button>
                {:else}
                  <button onclick={() => (confirmDelId = o.id)} class="flex-shrink-0 rounded-xl px-[12px] py-[8px] text-[12px] font-bold" style="border:none;background:var(--surface2);color:var(--faint)">Delete</button>
                {/if}
              </div>
            {/if}
          {:else if reviewedIds.has(o.id)}
            <div class="mt-[10px] flex items-center justify-between gap-[8px] border-t pt-[10px]" style="border-color:var(--line)">
              <span class="flex items-center gap-[6px] text-[12px] font-bold" style="color:var(--good)"><span>✓</span> Marked. I'll remember that.</span>
              <button onclick={() => unreview(o)} disabled={acting} class="flex-shrink-0 rounded-lg px-[12px] py-[6px] text-[11.5px] font-extrabold disabled:opacity-50" style="border:none;background:var(--surface2);color:var(--muted)">Not checked after all</button>
            </div>
          {/if}
        </div>
      {/each}
    </div>

    {#if nextCursor}
      <button onclick={() => loadPage(false)} disabled={loadingMore} class="mt-[14px] w-full rounded-[16px] border py-[12px] text-[13px] font-extrabold disabled:opacity-50" style="background:var(--surface);border-color:var(--line);color:var(--accent)">
        {loadingMore ? 'Loading...' : 'Load more'}
      </button>
    {/if}
  {/if}
{/snippet}

<div class="fade px-[18px] pt-[14px] pb-[120px] lg:mx-auto lg:max-w-[760px] lg:px-[32px] lg:pt-[24px] lg:pb-[60px] xl:max-w-[1240px]">
  <div class="flex items-center justify-between gap-[10px]">
    <div class="font-head text-[22px] font-semibold" style="color:var(--text)">Moments</div>
    <span class="lg:hidden"><GearButton {onnav} /></span>
  </div>
  <div class="mb-[14px] text-[13.5px]" style="color:var(--muted)">Watch the day back, search, and fix anything I got wrong.</div>

  {#if range}
    <div class="mb-[14px] flex items-center justify-between gap-[10px] rounded-[18px] border px-[14px] py-[10px]" style="background:var(--surface);border-color:var(--line)">
      <div class="min-w-0">
        <div class="text-[10.5px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Date range</div>
        <div class="font-head truncate text-[15px] font-semibold" style="color:var(--text)">{rangeLabel}</div>
      </div>
      <button onclick={exitRange} class="flex-shrink-0 rounded-full border px-[14px] py-[8px] text-[12px] font-extrabold" style="border-color:var(--line);background:var(--surface2);color:var(--accent)">Back to daily</button>
    </div>
  {:else}
    <DayNav />
  {/if}

  {#if layout.isWide}
    <!-- desktop: a persistent filter rail beside the results grid -->
    <div class="grid grid-cols-[288px_1fr] items-start gap-[24px]">
      <div class="flex flex-col">
        {@render summaryBanner()}
        {@render searchBox()}
        <div class="rounded-[22px] border p-[16px] pb-[4px]" style="background:var(--surface);border-color:var(--line)">
          {@render filterGroups()}
          {#if filterCount > 0 || search}
            <button onclick={clearAll} class="mb-[12px] w-full rounded-[14px] border py-[11px] text-[13px] font-extrabold" style="border-color:var(--line);background:transparent;color:var(--muted)">Clear all filters</button>
          {/if}
        </div>
      </div>
      <div class="min-w-0">
        <div class="mb-[14px] flex flex-wrap items-center gap-[10px]">
          <span class="text-[13px] font-bold" style="color:var(--text)">{#if total !== undefined}{total} {total === 1 ? 'moment' : 'moments'}{/if}</span>
          {@render sortToggle()}
          {#if activeChips.length}{@render activeChipsRow()}{/if}
        </div>
        {@render markAll()}
        {@render resultsBody()}
      </div>
    </div>
  {:else}
    {@render summaryBanner()}
    {@render markAll()}
    {@render searchBox()}

    <!-- filter controls -->
    <div class="mb-[12px] flex items-center gap-[9px]">
      <button onclick={() => (filterOpen = !filterOpen)} class="inline-flex flex-shrink-0 items-center gap-[7px] rounded-full border px-[15px] py-[9px] text-[13px] font-extrabold" style="background:var(--surface);border-color:var(--line);color:var(--text)">
        <svg viewBox="0 0 24 24" width="15" height="15" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round"><path d="M3 5h18M6 12h12M10 19h4" /></svg>
        Filters
        {#if filterCount > 0}<span class="inline-flex h-[18px] min-w-[18px] items-center justify-center rounded-full px-[5px] text-[10.5px] font-black" style="background:var(--accent);color:var(--ink)">{filterCount}</span>{/if}
      </button>
      {@render sortToggle()}
      <span class="min-w-0 flex-1 text-[12px] font-bold" style="color:var(--muted)">{#if total !== undefined}{total} {total === 1 ? 'moment' : 'moments'}{/if}</span>
      {#if filterCount > 0 || search}<button onclick={clearAll} class="flex-shrink-0 bg-transparent text-[12.5px] font-extrabold" style="border:none;color:var(--accent)">Clear</button>{/if}
    </div>

    {#if activeChips.length}
      <div class="mb-[14px]">{@render activeChipsRow()}</div>
    {/if}

    {#if filterOpen}
      <div class="fade mb-[16px] rounded-[22px] border p-[16px] pb-[12px]" style="background:var(--surface);border-color:var(--line)">
        {@render filterGroups()}
        <div class="mt-[4px] flex gap-[9px] border-t pt-[13px]" style="border-color:var(--line)">
          <button onclick={clearAll} class="flex-1 rounded-[14px] border py-[11px] text-[13px] font-extrabold" style="border-color:var(--line);background:transparent;color:var(--muted)">Clear all</button>
          <button onclick={() => (filterOpen = false)} class="flex-1 rounded-[14px] py-[11px] text-[13px] font-extrabold" style="border:none;background:var(--accent);color:var(--ink)">Done</button>
        </div>
      </div>
    {/if}

    {@render resultsBody()}
  {/if}
</div>
