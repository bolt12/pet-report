<script lang="ts">
  import Thumb from './Thumb.svelte'
  import Chip from './Chip.svelte'
  import WellbeingDot from './WellbeingDot.svelte'
  import { api, type AddSightingReq, type ObsView, type Pet } from './api'
  import { fmtWhen, friendlyError, serverMessage, BEHAVIOUR_FLAGS, type BehaviourFlag } from './ui'

  let {
    obs,
    pets,
    hasPrev = false,
    hasNext = false,
    onclose,
    onprev,
    onnext,
    onchanged,
  }: {
    obs: ObsView
    pets: Pet[]
    hasPrev?: boolean
    hasNext?: boolean
    onclose: () => void
    onprev: () => void
    onnext: () => void
    onchanged: () => void
  } = $props()

  // A locally refreshed copy of the moment. Adding or removing a subject changes the
  // sighting list and can renumber it, so the card has to re-read rather than keep showing
  // what the parent handed it. Cleared whenever the lightbox moves to another moment.
  let fresh = $state<ObsView | null>(null)
  $effect(() => {
    obs.id
    fresh = null
  })
  let view = $derived(fresh ?? obs)

  let note = $state('')
  let busy = $state(false)

  let confirmDel = $state(false)

  // The reviewed state is the persisted view.reviewed, with a session override so a
  // just-reviewed or just-undone moment updates instantly (the viewer works off a
  // snapshot that is not refreshed in place). `actedMsg` shows what it was set to;
  // `kept` tracks a saved keepsake so it too can be undone.
  let reviewedOverride = $state<boolean | null>(null)
  let actedMsg = $state('')
  let kept = $state<number | null>(null)
  let isReviewed = $derived(reviewedOverride ?? view.reviewed)
  // Kept either when opened (server flag) or just now (session id). A kept moment is
  // owned by pet-report, so it shows no clip-expiry countdown.
  let isKept = $derived(view.kept || kept !== null)
  // Whole days until Frigate is expected to prune this moment's borrowed footage, or
  // null when kept, unknown, or already gone.
  let daysLeft = $derived.by(() => {
    if (isKept || !view.clipExpiresAt) return null
    return Math.ceil((new Date(view.clipExpiresAt).getTime() - Date.now()) / 86400000)
  })

  // Transcription of a sound event (seeded from any cached transcript, then
  // filled on demand via Frigate/Whisper).
  let transcript = $state('')
  let transcribing = $state(false)
  let transcribeErr = $state('')
  // A clip/photo that fails to load (Frigate purged it, or a fetch error) falls back to
  // the placeholder rather than showing a broken element.
  let mediaBroken = $state(false)

  // Field-edit panel state, seeded from the current moment.
  let editing = $state(false)
  let eActivity = $state('unclear')
  let eWellbeing = $state('normal')
  let eDesc = $state('')
  let eWhere = $state('')
  let flags = $state<Record<BehaviourFlag, boolean>>({
    ate: false, drank: false, slept: false, played: false, groomed: false,
  })

  // The Activity enum; keep in step with backend Domain/Types.hs (Activity).
  const ACTIVITIES = [
    'sleeping', 'resting', 'sitting', 'standing', 'walking', 'running', 'jumping',
    'playing', 'eating', 'drinking', 'grooming', 'eliminating', 'alert', 'absent', 'unclear',
  ]
  const hasChip = (l: string) => view.chips.some((c) => c.label === l)

  function seedEdit() {
    eActivity = view.activity ?? 'unclear'
    eWellbeing = view.wellbeing === 'none' ? 'normal' : view.wellbeing
    eDesc = view.description ?? ''
    eWhere = view.location ?? ''
    for (const f of BEHAVIOUR_FLAGS) flags[f.key] = hasChip(f.key)
  }

  function onKey(e: KeyboardEvent) {
    if (e.key === 'Escape') onclose()
  }

  // Reset the transient note + edit panel and re-seed whenever the moment changes.
  $effect(() => {
    view.id
    note = ''
    editing = false
    confirmDel = false
    reviewedOverride = null
    actedMsg = ''
    kept = null
    transcript = view.transcript ?? ''
    transcribeErr = ''
    mediaBroken = false
    seedEdit()
  })

  const flagStyle = (on: boolean) =>
    on
      ? 'background:rgba(163,192,143,0.24);color:var(--good)'
      : 'background:rgba(255,246,236,0.08);color:rgba(255,246,236,0.55)'

  // One place for the busy flag + error-to-note + reload skeleton every action
  // shares; the action itself sets any success state (a note, the confirmation).
  async function busyDo(action: () => Promise<unknown>, close = false) {
    busy = true
    note = ''
    try {
      await action()
      onchanged()
      if (close) onclose()
    } catch (e) {
      note = friendlyError(e)
    } finally {
      busy = false
    }
  }
  const run = (fn: () => Promise<unknown>, msg: string, close = false) =>
    busyDo(async () => {
      await fn()
      note = msg
    }, close)

  // A review/correction settles the moment: mark it reviewed and note what it was
  // set to. Undo reverts to the model's original reading and re-flags it.
  const act = (fn: () => Promise<unknown>, message: string, after?: () => void) =>
    busyDo(async () => {
      await fn()
      reviewedOverride = true
      actedMsg = message
      after?.()
    })

  // A structural change re-reads the moment, because the sighting list it renders has
  // moved: a removal renumbers everything after it. onchanged() also tells the screen
  // underneath, so its card agrees once the lightbox closes.
  const structural = (fn: () => Promise<unknown>, message: string, after?: () => void) =>
    act(
      async () => {
        await fn()
        fresh = await api.moment(view.id)
        onchanged()
      },
      message,
      after,
    )
  const undoReview = () =>
    busyDo(async () => {
      await api.revert(view.id)
      reviewedOverride = false
      actedMsg = ''
    })
  const confirmOk = () => act(() => api.review([view.id]), 'Confirmed.')

  // Which sighting the corrections and the edit apply to. A moment holding two cats and a
  // sitter is three sightings, and each carries its own identity, so naming one must not
  // touch the others. Defaults to the first animal, which is the whole story for the
  // single-subject moment that most of them are.
  let subjectIx = $state(0)
  $effect(() => {
    // Re-seed whenever the moment changes, preferring an animal over a person.
    view.id
    const firstAnimal = view.subjects.find((s) => !s.person)
    subjectIx = firstAnimal?.ix ?? view.subjects[0]?.ix ?? 0
  })
  let subjectName = $derived(view.subjects.find((s) => s.ix === subjectIx)?.label ?? 'this one')

  // Adding a subject the model missed, and removing one it invented. Both reload the
  // moment, since the server may renumber the sightings.
  let addingSubject = $state(false)
  // The species worth offering: the household's own, plus cat and dog as the common
  // strays, deduplicated.
  let speciesChoices = $derived([...new Set([...pets.map((p) => p.petSpecies), 'cat', 'dog'])])
  const addSubject = (req: AddSightingReq) =>
    structural(() => api.addSighting(view.id, req), 'Added.', () => (addingSubject = false))
  const dropSubject = (ix: number) => structural(() => api.removeSighting(view.id, ix), 'Removed.')

  // Naming a subject changes the label the list above shows, so these re-read too.
  const reclass = (petId: string, name: string) =>
    structural(() => api.correct(view.id, subjectIx, { kind: 'pet', petId }), `Marked as ${name}.`)
  const asVisitor = () =>
    structural(() => api.correct(view.id, subjectIx, { kind: 'visiting' }), 'Marked as not your pet.')
  const asPerson = () =>
    structural(() => api.correct(view.id, subjectIx, { kind: 'person' }), 'Marked as a person.')
  const remove = () => run(() => api.del(view.id), 'Deleted.', true)
  const keep = () =>
    busyDo(async () => {
      const k = await api.keep(view.id, { petId: view.subjects[0]?.petId ?? undefined })
      kept = k.id
    })
  function undoKeep() {
    const id = kept
    if (id !== null)
      busyDo(async () => {
        await api.unkeep(id)
        kept = null
      })
  }
  async function doTranscribe() {
    if (transcribing) return
    transcribing = true
    transcribeErr = ''
    try {
      transcript = (await api.transcribe(view.id)).transcript
      onchanged() // persist into the list so the transcript survives a reopen
    } catch (e) {
      // Show Frigate's actual reason (e.g. transcription is off) rather than a generic
      // "try again", which this failure is not.
      transcribeErr = serverMessage(e)
    } finally {
      transcribing = false
    }
  }
  const saveEdit = () =>
    busyDo(async () => {
      await api.edit(view.id, subjectIx, {
        activity: eActivity,
        wellbeing: eWellbeing,
        description: eDesc,
        whereAt: eWhere,
        ...flags,
      })
      editing = false
      reviewedOverride = true
      actedMsg = 'Saved your changes.'
    })

  const mediaLabel = $derived(
    view.media.kind === 'expired'
      ? 'clip tidied away'
      : view.media.kind === 'audio'
        ? 'audio, sound only'
        : `${view.media.kind} · ${view.room.toLowerCase()}`,
  )
</script>

<svelte:window onkeydown={onKey} />

<div
  class="fixed inset-0 z-30 flex flex-col fade"
  style="background:rgba(15,11,8,0.82);backdrop-filter:blur(6px)"
>
  <div class="flex items-center justify-between px-[18px] pt-[18px] pb-[12px]">
    <button
      onclick={onclose}
      class="flex h-[38px] w-[38px] items-center justify-center rounded-full text-[18px] text-white"
      style="background:rgba(255,246,236,0.14)"
      aria-label="Close">✕</button
    >
    <span
      class="text-[12px] font-extrabold tracking-wider uppercase"
      style="color:rgba(255,246,236,0.6)">Moment</span
    >
    <span class="w-[38px]"></span>
  </div>

  <div class="flex-1 overflow-y-auto px-[18px] pb-[24px] lg:flex lg:flex-row lg:items-stretch lg:gap-[22px] lg:overflow-hidden lg:px-[24px]" data-scroll>
    <!-- media pane: fills its side on desktop, letterboxing the media -->
    <div class="lg:flex lg:min-w-0 lg:flex-1 lg:items-center lg:justify-center lg:overflow-hidden">
    <div
      class="relative mb-4 aspect-[4/3] w-full overflow-hidden rounded-[22px] lg:mb-0 lg:aspect-auto lg:h-full lg:w-full"
      style="box-shadow:0 20px 50px -10px rgba(0,0,0,0.6)"
    >
      {#if view.media.clipUrl && !mediaBroken}
        <!-- Only clip and audio-with-recording carry a clip URL, and a sound
             event's clip is an mp4 with a video track too, so the same player
             shows the scene and plays the audio. -->
        <!-- svelte-ignore a11y_media_has_caption -->
        <video
          src={view.media.clipUrl}
          poster={view.media.stillUrl ?? undefined}
          controls
          playsinline
          preload="metadata"
          class="absolute inset-0 h-full w-full"
          style="object-fit:contain;background:#12100e"
          onerror={() => (mediaBroken = true)}
        ></video>
      {:else if view.media.kind === 'photo' && view.media.stillUrl && !mediaBroken}
        <img
          src={view.media.stillUrl}
          alt=""
          class="absolute inset-0 h-full w-full"
          style="object-fit:contain;background:#12100e"
          onerror={() => (mediaBroken = true)}
        />
      {:else}
        <Thumb img={view.media.stillUrl} media={view.media.kind} room={view.room} pawSize={72} />
        <span
          class="absolute bottom-[11px] left-[12px] rounded-[7px] px-[8px] py-[3px] font-mono"
          style="font-size:10px;color:rgba(255,246,236,0.8);background:rgba(20,14,10,0.45)"
          >{mediaLabel}</span
        >
      {/if}
      {#if hasPrev}
        <button
          onclick={onprev}
          class="absolute top-1/2 left-[8px] flex h-[36px] w-[36px] -translate-y-1/2 items-center justify-center rounded-full text-[18px] text-white"
          style="background:rgba(20,14,10,0.5)"
          aria-label="Previous">‹</button
        >
      {/if}
      {#if hasNext}
        <button
          onclick={onnext}
          class="absolute top-1/2 right-[8px] flex h-[36px] w-[36px] -translate-y-1/2 items-center justify-center rounded-full text-[18px] text-white"
          style="background:rgba(20,14,10,0.5)"
          aria-label="Next">›</button
        >
      {/if}
    </div>
    </div>

    <!-- details pane: scrolls independently beside the media on desktop -->
    <div class="lg:w-[400px] lg:flex-shrink-0 lg:overflow-y-auto lg:pt-[2px] lg:pr-[4px]" data-scroll>
    <div class="mb-[8px] flex items-center gap-[8px]">
      <WellbeingDot wellbeing={view.wellbeing} size={9} />
      <span class="font-head text-[15px] font-semibold" style="color:#f4ece3">{fmtWhen(view.at)}</span>
      <span style="color:rgba(255,246,236,0.4)">·</span>
      <span class="text-[14px] font-semibold" style="color:rgba(255,246,236,0.7)">{view.room}</span>
    </div>

    <div class="mb-[10px] flex flex-wrap items-center gap-[9px]">
      <span class="font-head text-[24px] font-bold" style="color:#f4ece3">{view.subjectLabel}</span>
      {#if view.activity}
        <span
          class="rounded-full px-[11px] py-[3px] text-[12px] font-bold capitalize"
          style="color:rgba(255,246,236,0.55);background:rgba(255,246,236,0.1)">{view.activity}</span
        >
      {/if}
    </div>

    {#if view.description}
      <div class="mb-[14px] text-[14.5px] leading-[1.55]" style="color:rgba(255,246,236,0.9)">
        {view.description}
      </div>
    {/if}

    {#if view.media.kind === 'audio'}
      <div class="mb-[16px] rounded-[14px] px-[14px] py-[12px]" style="background:rgba(255,246,236,0.06)">
        {#if transcript}
          <div class="mb-[4px] text-[10.5px] font-extrabold tracking-wide uppercase" style="color:rgba(255,246,236,0.45)">Transcript</div>
          <div class="text-[14px] leading-[1.5]" style="color:rgba(255,246,236,0.92)">"{transcript}"</div>
        {:else}
          <button onclick={doTranscribe} disabled={transcribing} class="w-full rounded-2xl py-[11px] text-[13px] font-bold disabled:opacity-60" style="background:rgba(236,171,130,0.18);color:var(--accent)">{transcribing ? 'Listening...' : 'Transcribe speech'}</button>
        {/if}
        {#if transcribeErr}<div class="mt-[8px] text-[12px]" style="color:#e26d5c">{transcribeErr}</div>{/if}
      </div>
    {/if}

    {#if view.chips.length}
      <div class="mb-[16px] flex flex-wrap gap-[7px]">
        {#each view.chips as c, i (i)}
          <Chip label={c.label} kind={c.kind} nowrap />
        {/each}
      </div>
    {/if}

    {#if view.uncertain && view.confidence != null}
      <div
        class="mb-[16px] flex items-center gap-[9px] rounded-[14px] px-[14px] py-[11px]"
        style="background:rgba(183,168,192,0.12);border:1px solid rgba(183,168,192,0.28)"
      >
        <span
          class="h-[8px] w-[8px] flex-shrink-0 rounded-full"
          style="background:var(--unclear)"
        ></span>
        <span class="text-[13px]" style="color:rgba(255,246,236,0.85)"
          >I'm only <b>{Math.round(view.confidence * 100)}%</b> sure who this is. Help me learn?</span
        >
      </div>
    {/if}

    {#if note}
      <div class="mb-3 text-[13px] font-bold" style="color:var(--good)">✓ {note}</div>
    {/if}

    {#if isReviewed}
      <div class="mb-3 flex items-center gap-[6px] text-[13px] font-bold" style="color:var(--good)"><span>✓</span> {actedMsg || 'Reviewed'}</div>
    {/if}
    {#if confirmDel}
      <div class="mb-2 text-[12.5px] leading-[1.45]" style="color:#e6a08c">Deleting this moment removes it for good, along with any keepsake of it and its saved image. This can't be undone.</div>
    {/if}
    {#if isKept}
      <div class="mb-2 flex items-center gap-[6px] text-[12.5px] font-bold" style="color:var(--good)">♥ Kept in your moments</div>
    {:else if daysLeft !== null && daysLeft > 0}
      <!-- Only a forward-looking nudge: a moment already past its window is still shown
           until the next cleanup, so never claim a visible moment is "gone". -->
      <div class="mb-2 text-[12.5px]" style="color:{daysLeft <= 3 ? '#e6a08c' : 'rgba(255,246,236,0.55)'}">
        {daysLeft === 1
          ? 'This moment is tidied away within a day unless you Keep it.'
          : `This moment is tidied away in ${daysLeft} days unless you Keep it.`}
      </div>
    {/if}
    <div class="flex flex-wrap gap-[8px]">
      {#if isReviewed}
        <button
          onclick={undoReview}
          disabled={busy}
          class="flex-1 rounded-2xl py-[13px] text-[13.5px] font-extrabold disabled:opacity-50"
          style="background:rgba(236,171,130,0.18);color:var(--accent)">Undo</button
        >
      {:else}
        <button
          onclick={confirmOk}
          disabled={busy}
          class="flex-1 rounded-2xl py-[13px] text-[13.5px] font-extrabold disabled:opacity-50"
          style="background:rgba(163,192,143,0.2);color:var(--good)">That's right</button
        >
      {/if}
      {#if kept !== null}
        <button
          onclick={undoKeep}
          disabled={busy}
          class="rounded-2xl px-[16px] py-[13px] text-[13.5px] font-bold disabled:opacity-50"
          style="background:rgba(163,192,143,0.2);color:var(--good)">♥ Saved · Undo</button
        >
      {:else if view.kept}
        <button
          disabled
          class="rounded-2xl px-[16px] py-[13px] text-[13.5px] font-bold opacity-70"
          style="background:rgba(163,192,143,0.2);color:var(--good)">♥ Saved</button
        >
      {:else}
        <button
          onclick={keep}
          disabled={busy}
          class="rounded-2xl px-[16px] py-[13px] text-[13.5px] font-bold disabled:opacity-50"
          style="background:rgba(236,171,130,0.18);color:var(--accent)">Keep</button
        >
      {/if}
      {#if confirmDel}
        <button
          onclick={remove}
          disabled={busy}
          class="rounded-2xl px-[16px] py-[13px] text-[13.5px] font-bold disabled:opacity-50"
          style="background:rgba(226,109,92,0.22);color:#ffb3a6">Really delete</button
        >
        <button
          onclick={() => (confirmDel = false)}
          disabled={busy}
          class="rounded-2xl px-[16px] py-[13px] text-[13.5px] font-bold disabled:opacity-50"
          style="background:rgba(255,246,236,0.1);color:rgba(255,246,236,0.6)">Cancel</button
        >
      {:else}
        <button
          onclick={() => (confirmDel = true)}
          disabled={busy}
          class="rounded-2xl px-[16px] py-[13px] text-[13.5px] font-bold disabled:opacity-50"
          style="background:rgba(255,246,236,0.1);color:rgba(255,246,236,0.6)">Delete</button
        >
      {/if}
    </div>

    {#if !isReviewed}
    <!-- The moment's subjects, always shown. A frame can hold a pet AND a person, so this
         is a list you can add to and remove from, not a single choice. Tapping one selects
         it for the correction and edit controls below. -->
    <div class="mt-3 text-[11px] font-bold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">
      Who's in this one?
    </div>
    <div class="mt-2 flex flex-wrap items-center gap-[8px]">
      {#each view.subjects as s (s.ix)}
        <span class="inline-flex items-center gap-[6px] rounded-full px-[12px] py-[7px] text-[13px] font-bold"
          style={s.ix === subjectIx
            ? 'background:var(--accent);color:var(--ink)'
            : 'background:rgba(255,246,236,0.1);color:#f4ece3'}>
          <button onclick={() => (subjectIx = s.ix)} disabled={busy} style="border:none;background:none;padding:0;font:inherit;color:inherit;cursor:pointer">{s.label}</button>
          <button
            onclick={() => dropSubject(s.ix)}
            disabled={busy}
            title="Not actually there"
            style="border:none;background:none;padding:0;font:inherit;color:inherit;opacity:.65;cursor:pointer">&times;</button>
        </span>
      {/each}
      {#if addingSubject}
        <span class="inline-flex flex-wrap items-center gap-[6px]">
          <button onclick={() => addSubject({ kind: 'person' })} disabled={busy} class="rounded-full px-[12px] py-[7px] text-[13px] font-bold" style="background:rgba(255,246,236,0.1);color:#f4ece3">A person</button>
          {#each speciesChoices as sp (sp)}
            <button onclick={() => addSubject({ kind: 'species', species: sp })} disabled={busy} class="rounded-full px-[12px] py-[7px] text-[13px] font-bold capitalize" style="background:rgba(255,246,236,0.1);color:#f4ece3">A {sp}</button>
          {/each}
          <button onclick={() => (addingSubject = false)} disabled={busy} class="text-[12px] font-bold" style="border:none;background:none;color:rgba(255,246,236,0.55)">Cancel</button>
        </span>
      {:else}
        <button onclick={() => (addingSubject = true)} disabled={busy} class="rounded-full px-[12px] py-[7px] text-[13px] font-bold" style="background:rgba(255,246,236,0.06);color:rgba(255,246,236,0.75);border:1px dashed rgba(255,246,236,0.25)">+ Someone else</button>
      {/if}
    </div>
    <div class="mt-3 text-[11px] font-bold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">
      {view.subjects.length > 1 ? `Not right? Set who ${subjectName} is` : 'Not right? Set who it is'}
    </div>
    <div class="mt-2 flex flex-wrap gap-[8px]">
      {#each pets.filter((p) => !view.subjects.some((s) => s.ix === subjectIx && s.petId === p.petId)) as p (p.petId)}
        <button
          onclick={() => reclass(p.petId, p.petName)}
          disabled={busy}
          class="rounded-full px-[14px] py-[8px] text-[13px] font-bold disabled:opacity-50"
          style="background:rgba(255,246,236,0.1);color:#f4ece3">It's {p.petName}</button
        >
      {/each}
      <button
        onclick={asVisitor}
        disabled={busy}
        class="rounded-full px-[14px] py-[8px] text-[13px] font-bold disabled:opacity-50"
        style="background:rgba(255,246,236,0.1);color:#f4ece3">Not my pet</button
      >
      <button
        onclick={asPerson}
        disabled={busy}
        class="rounded-full px-[14px] py-[8px] text-[13px] font-bold disabled:opacity-50"
        style="background:rgba(255,246,236,0.1);color:#f4ece3">A person</button
      >
    </div>
    {/if}

    {#if view.media.kind !== 'audio'}
      <button
        onclick={() => (editing = !editing)}
        class="mt-3 w-full rounded-2xl py-[12px] text-[13px] font-bold"
        style="background:rgba(255,246,236,0.08);color:rgba(255,246,236,0.75)"
        >{editing ? 'Close edit' : '✎ Edit details'}</button
      >
      {#if editing}
        <div class="fade mt-3 rounded-[18px] p-[14px]" style="background:rgba(255,246,236,0.06)">
          <div class="mb-[10px] grid grid-cols-2 gap-[9px]">
            <label class="flex flex-col gap-[5px]">
              <span class="text-[10.5px] font-extrabold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">Doing</span>
              <select bind:value={eActivity} class="rounded-[10px] px-[10px] py-[9px] text-[13px] outline-none" style="background:rgba(20,14,10,0.6);color:#f4ece3;border:1px solid rgba(255,246,236,0.14)">
                {#each ACTIVITIES as a (a)}<option value={a}>{a}</option>{/each}
              </select>
            </label>
            <label class="flex flex-col gap-[5px]">
              <span class="text-[10.5px] font-extrabold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">Wellbeing</span>
              <select bind:value={eWellbeing} class="rounded-[10px] px-[10px] py-[9px] text-[13px] outline-none" style="background:rgba(20,14,10,0.6);color:#f4ece3;border:1px solid rgba(255,246,236,0.14)">
                <option value="normal">normal</option>
                <option value="concerning">concerning</option>
                <option value="unclear">unclear</option>
              </select>
            </label>
          </div>
          <label class="mb-[10px] flex flex-col gap-[5px]">
            <span class="text-[10.5px] font-extrabold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">Where</span>
            <input bind:value={eWhere} placeholder="e.g. on the sofa" class="rounded-[10px] px-[11px] py-[9px] text-[13px] outline-none" style="background:rgba(20,14,10,0.6);color:#f4ece3;border:1px solid rgba(255,246,236,0.14)" />
          </label>
          <label class="mb-[10px] flex flex-col gap-[5px]">
            <span class="text-[10.5px] font-extrabold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">Note</span>
            <textarea bind:value={eDesc} rows="2" placeholder="What's happening here" class="resize-none rounded-[10px] px-[11px] py-[9px] text-[13px] outline-none" style="background:rgba(20,14,10,0.6);color:#f4ece3;border:1px solid rgba(255,246,236,0.14)"></textarea>
          </label>
          <div class="mb-[13px] flex flex-wrap gap-[7px]">
            {#each BEHAVIOUR_FLAGS as f (f.key)}
              <button onclick={() => (flags[f.key] = !flags[f.key])} class="rounded-full px-[12px] py-[6px] text-[12px] font-bold" style={flagStyle(flags[f.key])}>{f.label}</button>
            {/each}
          </div>
          {#if view.subjects.length > 1}
            <div class="mb-[10px] text-[11px] leading-[1.4]" style="color:rgba(255,246,236,0.5)">Activity and behaviours apply to {subjectName}. The note and wellbeing cover the whole moment.</div>
          {/if}
          <button onclick={saveEdit} disabled={busy} class="w-full rounded-2xl py-[12px] text-[13.5px] font-extrabold disabled:opacity-50" style="background:var(--accent);color:var(--ink)">Save &amp; confirm</button>
          <div class="mt-[7px] text-center text-[11px]" style="color:rgba(255,246,236,0.45)">Saving also marks this moment as checked.</div>
        </div>
      {/if}
    {/if}
    </div>
  </div>
</div>
