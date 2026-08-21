<script lang="ts">
  import {
    api,
    photoUrlFor,
    type DayResponse,
    type Overview,
    type PetInsights,
    type Profile,
    type ObsView,
    type ReviewPreset,
  } from './api'
  import Avatar from './Avatar.svelte'
  import Paw from './Paw.svelte'
  import Thumb from './Thumb.svelte'
  import LiveView from './LiveView.svelte'
  import DayNav from './DayNav.svelte'
  import GearButton from './GearButton.svelte'
  import TimeText from './TimeText.svelte'
  import Chip from './Chip.svelte'
  import WellbeingDot from './WellbeingDot.svelte'
  import { openLightbox } from './lightbox.svelte'
  import { live as liveOverlay, openLive, closeLive } from './liveview.svelte'
  import { day } from './day.svelte'
  import { refreshes } from './refresh.svelte'
  import { greeting, longDate, dayDate, chipFg, chipStyle, roomTint, fmtTime, friendlyError, frameSrc, ymdForOffset } from './ui'
  import { toggleTheme, theme } from './theme.svelte'
  import { layout } from './layout.svelte'
  import { onDestroy } from 'svelte'

  let { profile, onnav }: { profile: Profile; onnav: (s: string, arg?: string | ReviewPreset) => void } = $props()


  let data = $state<DayResponse | null>(null)
  let overview = $state<Overview | null>(null)
  let glance = $state<PetInsights[]>([])
  let error = $state('')
  // Refresh state is per-day in the shared store, keyed by a stable CONCRETE date (never
  // the 'today' sentinel day.iso returns for offset 0), so the button and the mount effect
  // agree and refreshing one day never spins another's button.
  let dayKey = $derived(ymdForOffset(day.offset))
  let refreshing = $derived(refreshes.isRefreshing(dayKey))
  let refreshed = $derived(refreshes.updatedIso === dayKey)
  // False when this day still has camera events the app has not worked through, so its
  // moments and its story are both incomplete. Defaults to true so nothing flashes up
  // before the first check answers.
  let caughtUp = $state(true)
  let dismissed = $state(false)
  let recapText = $state('')
  let recapBusy = $state(false)

  // Flipped on unmount so an in-flight refresh poll stops and never writes state
  // into a destroyed component.
  let alive = true
  onDestroy(() => (alive = false))

  async function load() {
    const iso = day.iso // capture: a slow response must not overwrite a day we left
    error = ''
    try {
      // The day's moments and the per-pet glance for that day, fetched together.
      // A past day gets its glance from the dated endpoint so its cards reflect it.
      const [d, g, s] = await Promise.all([
        api.day(iso),
        api.insights(day.offset === 0 ? undefined : iso),
        // Whether this day's events have all been looked at yet. A failure here must never
        // break the day, so it degrades to hiding the notice.
        api.refreshStatus(day.offset === 0 ? undefined : iso).catch(() => null),
      ])
      if (!alive || day.iso !== iso) return
      data = d
      glance = g
      caughtUp = s?.caughtUp ?? true
      if (day.offset === 0) {
        // Live cameras and the earliest-day bound only make sense for today.
        overview = await api.overview()
        if (!alive || day.iso !== iso) return
        day.setEarliest(overview?.earliestDay)
      }
    } catch (e) {
      if (alive && day.iso === iso) error = friendlyError(e)
    }
  }
  $effect(() => {
    day.offset
    refreshes.updatedIso = null // a prior day's "Just updated" should not follow navigation
    load()
  })

  // On first mount (no reactive day read, so this runs once): if today's scheduled batch
  // is mid-run, adopt it so today's button reflects it and reloads when it finishes. Past
  // days are only built by an explicit click.
  $effect(() => {
    api
      .refreshStatus()
      .then((s) => {
        if (s.running && alive) runRefresh(ymdForOffset(0), undefined, false)
      })
      .catch(() => {})
  })

  // Poll a day's batch flag until its run finishes, with a safety cap so a hung run can
  // never wedge the spinner; stops early if the view is unmounted.
  //
  // The cap has to sit above the server's own batch deadline (20 minutes), or it fires on
  // runs that are merely long: the spinner clears while the run is still working, and the
  // outcome read afterwards is the PREVIOUS run's record, since this one has not written
  // its own yet.
  async function pollUntilIdle(arg?: string) {
    const deadline = Date.now() + 25 * 60 * 1000
    while (alive && Date.now() < deadline) {
      await new Promise((r) => setTimeout(r, 2000))
      if (!alive) return
      let running = false
      try {
        running = (await api.refreshStatus(arg)).running
      } catch {
        return
      }
      if (!running) return
    }
  }

  // The one refresh lifecycle, shared by the button and the mount-time adopt: optionally
  // kick a run, poll THIS day's job to completion, reload if we are still on it, and record
  // an honest per-day outcome. `key` is the concrete-date store key (captured at call time,
  // so a mid-refresh day switch cannot cross wires); `arg` is the API day (undefined =
  // today); `trigger` is a user click vs adopting an already-running batch.
  async function runRefresh(key: string, arg: string | undefined, trigger: boolean) {
    if (refreshes.isRefreshing(key)) return
    refreshes.start(key)
    if (trigger) error = ''
    try {
      if (trigger) await api.refresh(arg)
      await pollUntilIdle(arg)
      if (alive && dayKey === key) await load()
      // Read the recorded outcome so a stalled/failed run is honest, not a silent "Just
      // updated". Only an 'ok' run earns the note ('skipped' means a scheduled batch held
      // the lock and the reload already reflects its work).
      const b = await api.refreshStatus(arg)
      if (b.last?.status === 'error') {
        if (trigger && alive && dayKey === key) error = 'The refresh hit a problem and may be incomplete.'
      } else if (b.last?.status === 'ok') {
        refreshes.updatedIso = key
      }
    } catch (e) {
      if (trigger && alive && dayKey === key) error = friendlyError(e)
    } finally {
      refreshes.stop(key)
    }
  }

  const refresh = () => runRefresh(dayKey, day.offset === 0 ? undefined : day.iso, true)

  async function catchUp() {
    recapBusy = true
    recapText = ''
    try {
      recapText = (await api.recap(2)).recap
    } catch (e) {
      recapText = friendlyError(e)
    } finally {
      recapBusy = false
    }
  }

  let obs = $derived(data?.moments ?? [])
  // The day's concerns still waiting on you, stated once so the heads-up and the badge cannot
  // disagree. Both are gated on needsReview, so reviewing one drops it from both and from the
  // list they open. The badge used to count moments no filter could retrieve, leaving it
  // nowhere to send you.
  //
  // It counts a SUBSET of that list, which also holds whatever the model was unsure about.
  // Identical today, since a missing confidence never trips the unsure threshold; they part
  // company once confidence is populated, and the honest fix then is a concerning facet in
  // Review rather than a looser count here.
  // The badge's count and the query its link opens, defined once. The count filters the
  // day's loaded moments; the query asks the server for the same set. Previously the link
  // fell back to the needs-a-look backlog, a strict superset of what the badge counted, so
  // the list was reliably longer than the number that opened it.
  let concerningQuery = $derived<ReviewPreset>({
    from: ymdForOffset(day.offset),
    to: ymdForOffset(day.offset),
    wellbeing: 'concerning',
    review: 'unreviewed',
  })
  let concerning = $derived(obs.filter((o) => o.wellbeing === 'concerning' && o.needsReview))
  // Likewise for the presence note. This is the link that used to carry a fabricated pet
  // id of 'visitor' and land on an empty page.
  let personQuery = $derived<ReviewPreset>({
    from: ymdForOffset(day.offset),
    to: ymdForOffset(day.offset),
    subject: [{ kind: 'person' }],
  })
  let alert = $derived(day.offset === 0 && !dismissed ? concerning[0] : undefined)
  let concerningCount = $derived(concerning.length)
  let live = $derived(overview?.cameras ?? [])
  // The moments this day holds that are still queued: ones the model was unsure about, plus
  // ones flagged concerning. The same day-scoped rule for today and any past day (contract
  // D1). It used to show the global backlog on today, so one old moment read as if today had
  // one to review.
  let needsCount = $derived(obs.filter((o) => o.needsReview).length)
  // The review nudge counts this day's flagged-or-unsure moments, so its link says the
  // same thing: the needs-a-look backlog scoped to the day in view.
  let needsQuery = $derived<ReviewPreset>({
    from: ymdForOffset(day.offset),
    to: ymdForOffset(day.offset),
    review: 'needs-look',
  })

  // Cache-busting tick for the "Right now" stills, so they refresh instead of freezing on
  // the first frame. Runs only while those tiles are shown (today, with cameras) and pauses
  // while the tab is hidden, so we never poll frames nobody is watching.
  let frameTick = $state(0)
  $effect(() => {
    if (day.offset !== 0 || live.length === 0) return
    let id: ReturnType<typeof setInterval> | null = null
    const start = () => {
      if (id === null) id = setInterval(() => (frameTick += 1), 2000)
    }
    const stop = () => {
      if (id !== null) {
        clearInterval(id)
        id = null
      }
    }
    const onVis = () => (document.hidden ? stop() : start())
    onVis()
    document.addEventListener('visibilitychange', onVis)
    return () => {
      stop()
      document.removeEventListener('visibilitychange', onVis)
    }
  })

  const nice = new Set(['grooming', 'sleeping', 'eating', 'resting', 'drinking'])
  const notable = (o: ObsView) => o.uncertain || o.chips.some((c) => c.kind === 'watch' || c.kind === 'info')
  let hlNotable = $derived(obs.filter(notable).slice(0, 2))
  let hlNice = $derived(obs.filter((o) => o.activity && nice.has(o.activity) && !hlNotable.includes(o)).slice(0, 3))
  let highlights = $derived([...hlNotable, ...hlNice].slice(0, 5))

  function open(o: ObsView) {
    openLightbox(obs, o.id, profile.pets, load)
  }
</script>

<!-- Each block is defined once as a snippet, then rendered into either the mobile
     column (original order) or the desktop story/rail two-column grid. -->
{#snippet narrative()}
  <!-- narrative -->
  <div class="relative mb-[14px] overflow-hidden rounded-[26px] border p-[20px]" style="background:var(--surface);border-color:var(--line)">
    <div class="absolute -top-[14px] -right-[14px]" style="color:var(--accent);opacity:.12"><Paw size={92} /></div>
    <div class="mb-[11px] flex items-center justify-between gap-[8px]">
      <div class="flex items-center gap-[8px]"><span class="h-[8px] w-[8px] rounded-full" style="background:var(--accent);box-shadow:0 0 12px var(--accent)"></span><span class="font-head text-[16px] font-semibold" style="color:var(--text)">{day.offset === 0 ? 'Today so far' : 'Summary'}</span></div>
      {#if concerningCount > 0}
        <button onclick={() => onnav('review', concerningQuery)} class="tappable rounded-full px-[11px] py-[5px] text-[11.5px] font-extrabold" style="{chipStyle('watch')};border:none">{concerningCount} to check ›</button>
      {:else if obs.length === 0}
        <span class="rounded-full px-[11px] py-[5px] text-[11.5px] font-extrabold" style="background:rgba(255,246,236,0.06);color:var(--muted)">Nothing captured</span>
      {:else}
        <span class="rounded-full px-[11px] py-[5px] text-[11.5px] font-extrabold" style="background:rgba(163,192,143,0.16);color:var(--good)">All settled</span>
      {/if}
    </div>
    <div class="relative text-[15px] leading-[1.55]" style="color:var(--text);opacity:.92"><TimeText text={data?.narrative ?? 'No summary yet for this day. The cameras are often off, so a quiet log is perfectly normal.'} date={ymdForOffset(day.offset)} {onnav} /></div>
    {#if data?.presence?.someoneHome}
      <button onclick={() => onnav('review', personQuery)} class="mt-[11px] inline-flex items-center gap-[6px] text-[12px] font-semibold" style="background:none;border:none;padding:0;color:var(--muted)"><span style="opacity:.7">⌂</span> Someone was home {day.offset === 0 ? 'today' : 'that day'} <span style="color:var(--accent)">›</span></button>
    {/if}
    <div class="mt-[15px] flex flex-wrap gap-[9px]">
      <button onclick={refresh} disabled={refreshing} class="inline-flex items-center gap-[7px] rounded-full px-[16px] py-[9px] text-[13px] font-extrabold whitespace-nowrap disabled:opacity-70" style="background:var(--accent);color:var(--ink);border:none"><span style="display:inline-block;{refreshing ? 'animation:petSpin .9s linear infinite' : ''}">↻</span> {refreshing ? 'Looking...' : refreshed ? 'Just updated' : 'Refresh'}</button>
      {#if day.offset === 0}
        <button onclick={catchUp} disabled={recapBusy} class="rounded-full border px-[16px] py-[9px] text-[13px] font-bold whitespace-nowrap disabled:opacity-70" style="background:var(--surface2);border-color:var(--line);color:var(--text)">{recapBusy ? 'Catching up...' : 'Catch up · 2h'}</button>
      {/if}
    </div>
    {#if !refreshing && refreshes.otherRefreshing(dayKey)}
      <div class="mt-[9px] text-[11.5px]" style="color:var(--muted)">Another day is still updating. Refreshes run one at a time.</div>
    {/if}
    {#if recapText}<div class="mt-[13px] border-t pt-[13px] text-[14px] leading-[1.55]" style="border-color:var(--line);color:var(--text);opacity:.9"><TimeText text={recapText} date={ymdForOffset(day.offset)} {onnav} /></div>{/if}
  </div>
{/snippet}

{#snippet alertCard()}
  <!-- gentle heads-up -->
  {#if alert}
    <div class="fade mb-[14px] rounded-[22px] p-[15px]" style="background:linear-gradient(135deg,rgba(236,177,99,0.14),rgba(236,177,99,0.06));border:1px solid rgba(236,177,99,0.32)">
      <div class="mb-[5px] flex items-center gap-[8px]"><span class="h-[9px] w-[9px] rounded-full" style="background:var(--watch);animation:petPulse 2.4s ease infinite"></span><span class="text-[13px] font-extrabold" style="color:var(--watch)">Gentle heads-up · {alert.subjectLabel}</span></div>
      <div class="text-[14px] leading-[1.5]" style="color:var(--text);opacity:.92">Around <b>{fmtTime(alert.at)}</b> in the {alert.room.toLowerCase()}, {alert.description ?? 'something worth a glance.'}</div>
      <div class="mt-[12px] flex gap-[8px]"><button onclick={() => open(alert)} class="rounded-full px-[15px] py-[8px] text-[13px] font-extrabold" style="background:var(--watch);color:#3a2a10;border:none">Take a look</button><button onclick={() => (dismissed = true)} class="rounded-full border px-[15px] py-[8px] text-[13px] font-bold" style="border-color:var(--line);background:transparent;color:var(--muted)">Dismiss</button></div>
    </div>
  {/if}
{/snippet}

{#snippet glanceCards()}
  <!-- per-pet glance -->
  {#if glance.length}
    <div class="mb-[16px] flex flex-col gap-[10px] lg:grid lg:grid-cols-2 lg:gap-[12px]">
      {#each glance as p (p.id)}
        <button onclick={() => onnav('pets', p.id)} class="tappable fade flex w-full items-center gap-[13px] rounded-[22px] border p-[13px] text-left" style="background:var(--surface);border-color:var(--line);color:inherit">
          <Avatar name={p.name} species={p.species} size={54} photo={photoUrlFor(profile.pets, p.id)} />
          <div class="min-w-0 flex-1">
            <div class="flex items-baseline gap-[8px]"><span class="font-head text-[18px] font-semibold" style="color:var(--text)">{p.name}</span><span class="text-[12.5px] font-bold" style="color:{chipFg(p.note.kind)}">{p.note.label}</span></div>
            <div class="mt-[7px] flex flex-wrap gap-[6px]">{#each p.glance as c, i (i)}<Chip label={c.label} kind={c.kind} />{/each}</div>
          </div>
          <span class="flex-shrink-0 text-[20px]" style="color:var(--faint)">›</span>
        </button>
      {/each}
    </div>
  {/if}
{/snippet}

{#snippet thatDayPets()}
  <!-- that day's pets: the durable per-pet stats, shown once a day's raw moments have
       been tidied away (so an old day still tells you how each pet did) -->
  {#if data && glance.length === 0 && data.pets.length > 0}
    <div class="mb-[16px]">
      <div class="mx-[2px] mb-[10px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">That day's pets</div>
      <div class="flex flex-col gap-[8px]">
        {#each data.pets as s (s.petId ?? s.label)}
          <div class="fade flex items-center gap-[12px] rounded-[18px] border p-[12px]" style="background:var(--surface);border-color:var(--line)">
            <Avatar name={s.label} species={s.species} size={40} photo={photoUrlFor(profile.pets, s.petId)} />
            <div class="min-w-0 flex-1">
              <div class="font-head text-[15px] font-semibold" style="color:var(--text)">{s.label}</div>
              <div class="mt-[4px] text-[12px]" style="color:var(--muted)">Seen {s.sightings}×{#if s.rest > 0} · rested {s.rest}{/if}{#if s.active > 0} · active {s.active}{/if}{#if s.ate > 0} · ate {s.ate}{/if}{#if s.concerns > 0} · <span style="color:var(--watch)">{s.concerns} to watch</span>{/if}</div>
            </div>
          </div>
        {/each}
      </div>
    </div>
  {/if}
{/snippet}

{#snippet rightNow()}
  <!-- right now -->
  {#if day.offset === 0 && live.length}
    <div class="mb-[16px]">
      <div class="mx-[2px] mb-[10px] flex items-center gap-[8px]"><span class="h-[8px] w-[8px] rounded-full" style="background:#e26d5c;animation:petPulse 1.8s ease infinite"></span><span class="text-[12.5px] font-extrabold tracking-wider uppercase" style="color:var(--muted)">Right now</span></div>
      <div class="flex gap-[10px] overflow-x-auto pb-[2px] lg:flex-wrap lg:overflow-visible" data-scroll>
        {#each live as c (c.camera)}
          <button onclick={() => openLive(c.camera, c.room, c.online)} class="relative h-[86px] flex-shrink-0 overflow-hidden rounded-2xl border text-left" style="width:128px;border-color:var(--line);background:transparent;padding:0" aria-label={`Live view of ${c.room}`}>
            <div class="absolute inset-0" style="background:{roomTint(c.room)};{c.online ? '' : 'filter:grayscale(0.4);opacity:0.5'}"></div>
            {#if c.online}<img src={frameSrc(c.camera, frameTick)} alt="" class="absolute inset-0 h-full w-full object-cover" style="opacity:0;transition:opacity .18s" onload={(e) => ((e.currentTarget as HTMLImageElement).style.opacity = '1')} onerror={(e) => ((e.currentTarget as HTMLImageElement).style.opacity = '0')} />{/if}
            <div class="absolute inset-0 flex items-center justify-center" style="color:rgba(255,246,236,0.16)"><Paw size={30} /></div>
            <div class="absolute right-0 bottom-0 left-0 h-[38px]" style="background:linear-gradient(0deg,rgba(20,14,10,0.6),transparent)"></div>
            {#if c.online}<span class="absolute top-[8px] left-[8px] inline-flex items-center gap-[4px] rounded-full px-[6px] py-[2px] text-[9px] font-extrabold tracking-wide" style="background:rgba(20,14,10,0.5);color:#ffd9cf"><span class="h-[6px] w-[6px] rounded-full" style="background:#e26d5c;box-shadow:0 0 8px #e26d5c;animation:petPulse 1.8s ease infinite"></span>LIVE</span>{:else}<span class="absolute top-[8px] right-[8px] rounded-full px-[8px] py-[2px] text-[9px] font-extrabold" style="background:rgba(20,14,10,0.4);color:rgba(255,246,236,0.55)">OFF</span>{/if}
            <span class="absolute bottom-[8px] left-[10px] text-[11px] font-extrabold" style="color:rgba(255,246,236,0.95)">{c.room}</span>
          </button>
        {/each}
      </div>
    </div>
  {/if}
{/snippet}

{#snippet catchUpNotice()}
  <!-- shown only while this day still has events waiting to be looked at -->
  {#if !caughtUp}
    <button
      onclick={refresh}
      disabled={refreshing}
      class="tappable mb-[16px] flex w-full items-center gap-[12px] rounded-[20px] p-[14px_15px] text-left"
      style="background:rgba(236,177,99,0.1);border:1px solid rgba(236,177,99,0.3);color:inherit">
      <span class="flex h-[38px] w-[38px] flex-shrink-0 items-center justify-center rounded-xl" style="background:var(--watch);color:#3a2a10"><Paw size={20} /></span>
      <div class="min-w-0 flex-1">
        <div class="font-head text-[15.5px] font-semibold" style="color:var(--text)">Still catching up</div>
        <div class="mt-[1px] text-[12px]" style="color:var(--muted)">
          {refreshing
            ? 'Working through the rest now'
            : "This day still has camera events to look at, so its story may be incomplete. Tap to work through them; a busy day can take more than one go."}
        </div>
      </div>
      <span class="flex-shrink-0 text-[19px]" style="color:var(--faint)">›</span>
    </button>
  {/if}
{/snippet}

{#snippet needsNudge()}
  <!-- needs a look nudge -->
  <button onclick={() => onnav('review', needsCount > 0 ? needsQuery : undefined)} class="tappable mb-[16px] flex w-full items-center gap-[12px] rounded-[20px] p-[14px_15px] text-left" style="background:{needsCount > 0 ? 'rgba(236,177,99,0.1)' : 'rgba(163,192,143,0.1)'};border:1px solid {needsCount > 0 ? 'rgba(236,177,99,0.3)' : 'rgba(163,192,143,0.28)'};color:inherit">
    <span class="flex h-[38px] w-[38px] flex-shrink-0 items-center justify-center rounded-xl" style="background:{needsCount > 0 ? 'var(--watch)' : 'var(--good)'};color:{needsCount > 0 ? '#3a2a10' : '#1f3018'}">{#if needsCount > 0}<Paw size={20} />{:else}<span class="text-[17px] font-black">✓</span>{/if}</span>
    <div class="min-w-0 flex-1"><div class="font-head text-[15.5px] font-semibold" style="color:var(--text)">{needsCount > 0 ? `${needsCount} moment${needsCount === 1 ? '' : 's'} to review` : 'All caught up'}</div><div class="mt-[1px] text-[12px]" style="color:var(--muted)">{needsCount > 0 ? "A few I flagged or wasn't sure about, tap to take a look" : `Nothing needs your eyes ${day.offset === 0 ? 'today' : 'that day'}`}</div></div>
    <span class="flex-shrink-0 text-[19px]" style="color:var(--faint)">›</span>
  </button>
{/snippet}

{#snippet highlightsBlock()}
  <!-- highlights -->
  {#if highlights.length}
    <div class="mx-[2px] mt-[2px] mb-[12px] flex items-center justify-between">
      <span class="font-head text-[17px] font-semibold" style="color:var(--text)">{day.offset === 0 ? "Today's highlights" : 'Highlights'}</span>
      <button onclick={() => onnav('review')} class="bg-transparent text-[12.5px] font-bold" style="border:none;color:var(--accent)">See all {obs.length} ›</button>
    </div>
    <div class="flex gap-[11px] overflow-x-auto pb-[4px] lg:flex-wrap lg:overflow-visible" data-scroll>
      {#each highlights as o (o.id)}
        <button onclick={() => open(o)} class="tappable fade flex-shrink-0 rounded-[20px] border p-[9px] text-left" style="width:148px;background:var(--surface);border-color:var(--line);color:inherit">
          <div class="relative mb-[9px] h-[104px] overflow-hidden rounded-[14px]">
            <Thumb img={o.media.stillUrl} media={o.media.kind} room={o.room} pawSize={32} />
            {#if notable(o)}<span class="absolute top-[7px] right-[7px] h-[9px] w-[9px] rounded-full" style="background:var(--watch);box-shadow:0 0 8px var(--watch)"></span>{/if}
            <span class="absolute bottom-[6px] left-[7px] rounded-[6px] px-[6px] py-[2px] font-mono text-[9px]" style="color:rgba(255,246,236,0.85);background:rgba(20,14,10,0.5)">{fmtTime(o.at)}</span>
          </div>
          <div class="flex items-center gap-[6px] px-[3px]"><WellbeingDot wellbeing={o.wellbeing} /><span class="font-head text-[14px] font-semibold" style="color:var(--text)">{o.subjectLabel}</span></div>
          {#if o.description}<div class="clamp2 mt-[3px] px-[3px] text-[11.5px] leading-[1.35]" style="color:var(--muted)">{o.description}</div>{/if}
        </button>
      {/each}
    </div>
  {/if}
{/snippet}

{#snippet sparseBlock()}
  <!-- sparse -->
  {#if obs.length < 3}
    <div class="mt-[12px] rounded-[22px] border border-dashed p-[26px] text-center" style="background:var(--surface);border-color:var(--line)">
      <div class="mb-[8px] flex justify-center" style="color:var(--accent);opacity:.5"><Paw size={34} /></div>
      <div class="mx-auto max-w-[250px] text-[13.5px] leading-[1.5]" style="color:var(--muted)">{day.offset === 0 ? 'Nothing more captured just now. The cameras are often off, so this is perfectly normal. Check back later, or tap Refresh.' : 'Only a little was captured on this day. The cameras are often off, so a quiet day is perfectly normal.'}</div>
    </div>
  {/if}
{/snippet}

<div class="fade px-[18px] pt-[6px] pb-[120px] lg:mx-auto lg:max-w-[760px] lg:px-[34px] lg:pt-[26px] lg:pb-[64px] xl:max-w-[1200px]">
  <!-- header -->
  <div class="flex items-start justify-between gap-[10px] px-[2px] pt-[8px] pb-[14px]">
    <div>
      <div class="font-head text-[27px] leading-[1.05] font-semibold" style="color:var(--text)">{greeting()}</div>
      <!-- The date of the day being READ, not the date it happens to be. The greeting is a
           hello and stays wall-clock, but this line sat above a navigator saying "Mon, Aug
           17" and read "Friday, August 21", so the screen carried two dates that disagreed. -->
      <div class="mt-[3px] text-[13px] font-semibold" style="color:var(--muted)">{longDate(dayDate(day.offset))}</div>
    </div>
    <!-- theme + settings live in the sidebar on desktop, so hide them here -->
    <div class="flex flex-shrink-0 gap-[8px] lg:hidden">
      <button onclick={toggleTheme} class="flex h-[40px] w-[40px] items-center justify-center rounded-full border text-[16px]" style="border-color:var(--line);background:var(--surface);color:var(--text)" aria-label="Theme">{theme.name === 'night' ? '☾' : '☀'}</button>
      <GearButton {onnav} />
    </div>
  </div>

  <!-- day nav -->
  <DayNav />

  {#if error}<p class="mb-3 text-[13px]" style="color:#e26d5c">{error}</p>{/if}

  <!-- Above the story, in both layouts, because it is a caveat on the story itself. -->
  {@render catchUpNotice()}

  {#if layout.isWide}
    <!-- desktop: a wide story column beside a narrower rail -->
    <div class="grid grid-cols-[minmax(0,1.6fr)_minmax(320px,1fr)] items-start gap-[22px]">
      <div class="flex min-w-0 flex-col">
        {@render narrative()}
        {@render glanceCards()}
        {@render thatDayPets()}
        {@render highlightsBlock()}
        {@render sparseBlock()}
      </div>
      <div class="flex min-w-0 flex-col">
        {@render alertCard()}
        {@render needsNudge()}
        {@render rightNow()}
      </div>
    </div>
  {:else}
    {@render narrative()}
    {@render alertCard()}
    {@render glanceCards()}
    {@render thatDayPets()}
    {@render rightNow()}
    {@render needsNudge()}
    {@render highlightsBlock()}
    {@render sparseBlock()}
  {/if}

  {#if liveOverlay.open}
    <LiveView cam={liveOverlay.cam} room={liveOverlay.room} online={liveOverlay.online} onclose={closeLive} />
  {/if}
</div>
