<script lang="ts">
  import Thumb from './Thumb.svelte'
  import Chip from './Chip.svelte'
  import WellbeingDot from './WellbeingDot.svelte'
  import { api, type AddSightingReq, type CorrectReq, type ObsView, type Pet } from './api'
  import { fmtWhen, serverMessage, chipStyle, BEHAVIOUR_FLAGS, type BehaviourFlag } from './ui'

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
  // what the parent handed it.
  //
  // Re-read on OPEN too, not only after an edit. The lists deliberately show only what the
  // app counts, while this screen is where an unconfirmed naming gets settled, so opening a
  // moment has to ask for the fuller answer or the guess it exists to confirm never
  // arrives. The id is captured, so paging quickly cannot land one moment's data on another.
  let fresh = $state<ObsView | null>(null)
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
  // Display-only, so it follows the moment rather than being copied into state by an effect.
  let transcript = $derived(view.transcript ?? '')
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

  // Escape closes; the arrows step between moments, matching the on-screen chevrons. A
  // viewer you page through with a mouse but not with the keys beside it is a viewer that
  // forgot half its audience. Ignored while typing, so the edit panel keeps its arrow keys.
  // Seed on OPEN, never on every re-read. The panel now stays open across a subject change,
  // and that change re-reads the moment, so re-seeding whenever the moment changed threw away
  // whatever the owner had typed into the note beside it.
  function toggleEdit() {
    if (!editing) seedEdit()
    editing = !editing
  }

  function onKey(e: KeyboardEvent) {
    if (e.key === 'Escape') return onclose()
    const t = e.target as HTMLElement | null
    if (t && (t.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(t.tagName))) return
    if (e.key === 'ArrowLeft' && hasPrev) {
      e.preventDefault()
      onprev()
    } else if (e.key === 'ArrowRight' && hasNext) {
      e.preventDefault()
      onnext()
    }
  }

  // Everything that happens when the viewer moves to ANOTHER moment: reset, then re-read.
  //
  // Keyed on the incoming prop, not on `view`. `view` also changes when an edit re-reads
  // this same moment, and keying on that wiped the confirmation the edit had just written.
  //
  // The re-read is unconditional, not only after an edit: the lists deliberately show only
  // what the app counts, while this screen is where an unconfirmed naming gets settled, so
  // opening a moment has to ask for the fuller answer or the guess it exists to confirm
  // never arrives. The id is captured and the fetch is debounced, so holding an arrow key
  // pages without firing a request per moment stepped past, and a late reply for a moment
  // already left behind is dropped rather than shown.
  $effect(() => {
    const id = obs.id
    fresh = null
    note = ''
    editing = false
    confirmDel = false
    reviewedOverride = null
    actedMsg = ''
    kept = null
    transcribeErr = ''
    mediaBroken = false
    editingIx = null
    let live = true
    const t = setTimeout(() => {
      api
        .moment(id)
        .then((m) => {
          if (live && m.id === id) fresh = m
        })
        .catch(() => {}) // the handed-down copy is a fine fallback; nothing is worse off
    }, 150)
    return () => {
      live = false
      clearTimeout(t)
    }
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
      // serverMessage, not friendlyError: the cap refusal and every other owner-facing 400
      // this app writes arrives here, and friendlyError replaces all of them with the same
      // "The server hit a problem" line.
      note = serverMessage(e)
    } finally {
      busy = false
    }
  }
  const run = (fn: () => Promise<unknown>, msg: string, close = false) =>
    busyDo(async () => {
      await fn()
      note = msg
    }, close)

  // The owner's verdict, and the only thing that settles the moment. Re-reads afterwards
  // because confirming also adopts any name the model had only guessed at, and a row still
  // captioned "my guess" under a moment the owner has just agreed with is the app arguing
  // with itself.
  const confirmOk = () =>
    busyDo(async () => {
      await api.review([view.id])
      fresh = await api.moment(view.id)
      reviewedOverride = true
      actedMsg = 'Confirmed.'
    })

  // A change to the reading: who is here, and who is not. It deliberately does NOT settle
  // the moment, so the panel below stays open for the next change and "That's right" still
  // means something. Re-reads the moment, because the sighting list it renders has moved: a
  // removal renumbers everything after it. onchanged() also tells the screen underneath, so
  // its card agrees once the lightbox closes.
  const structural = (fn: () => Promise<unknown>, message: string, after?: () => void) =>
    busyDo(async () => {
      await fn()
      fresh = await api.moment(view.id)
      onchanged()
      note = message
      after?.()
    })

  // Take the verdict back, keeping every subject the owner named. `revert` is the other,
  // wider undo, offered separately below, and it discards those corrections too.
  const undoReview = () =>
    busyDo(async () => {
      await api.unreview(view.id)
      reviewedOverride = false
      actedMsg = ''
    })
  // Throw away everything said about this moment and go back to the model's own reading.
  const startOver = () =>
    busyDo(async () => {
      await api.revert(view.id)
      fresh = await api.moment(view.id)
      reviewedOverride = false
      actedMsg = ''
      note = 'Back to what I first saw.'
    })

  // Which subject row has its options open. Presentation only: a correction is addressed
  // by the row it came from, never by a selection held somewhere else. Reset with everything
  // else when the viewer moves on; a retarget clears it at the point it happens.
  let editingIx = $state<number | null>(null)

  // The field edit still applies to one sighting. It targets the first animal, since the
  // activity and behaviour flags it carries are animal facts; the note and wellbeing it
  // also sets are scene-level and land whatever is addressed.
  let editIx = $derived(view.subjects.find((s) => !s.person)?.ix ?? view.subjects[0]?.ix ?? 0)
  let editSubjectName = $derived(view.subjects.find((s) => s.ix === editIx)?.label ?? 'this one')

  // Adding a subject the model missed, and removing one it invented. Both reload the
  // moment, since the server may renumber the sightings.
  let addingSubject = $state(false)
  // Whether a repeat would be a SECOND of something, so the add panel can say so. How many
  // is too many is the server's to decide and the server's to explain: it answers a 400
  // naming its own cap, which `note` now shows verbatim, so holding a second copy of the
  // number here would only be a chance to disagree with it.
  let hasPerson = $derived(view.subjects.some((s) => s.person))
  const hasSpecies = (sp: string) => view.subjects.some((s) => !s.person && s.species === sp)
  // The species worth offering: the household's own, plus cat and dog as the common
  // strays, deduplicated.
  let speciesChoices = $derived([...new Set([...pets.map((p) => p.petSpecies), 'cat', 'dog'])])
  const addSubject = (req: AddSightingReq) =>
    structural(() => api.addSighting(view.id, req), 'Added.', () => (addingSubject = false))
  const dropSubject = (ix: number) => structural(() => api.removeSighting(view.id, ix), 'Removed.')

  // Each takes the row it came from. Naming a subject changes the label that row shows,
  // so they re-read the moment; the open row closes because the options it offered no
  // longer describe what is there.
  const retarget = (ix: number, req: CorrectReq, message: string) =>
    structural(() => api.correct(view.id, ix, req), message, () => (editingIx = null))
  const reclass = (ix: number, petId: string, name: string) =>
    retarget(ix, { kind: 'pet', petId }, `Marked as ${name}.`)
  const asNotMine = (ix: number) => retarget(ix, { kind: 'visiting' }, 'Marked as not your pet.')
  // Agreeing with the model's guess for one row. Goes through the same correction the other
  // buttons use, so it settles that sighting and nothing else.
  const keepGuess = (ix: number, petId: string, name: string) =>
    structural(() => api.correct(view.id, ix, { kind: 'pet', petId }), `Marked as ${name}.`)
  const asPerson = (ix: number) => retarget(ix, { kind: 'person' }, 'Marked as a person.')
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
      await api.transcribe(view.id)
      fresh = await api.moment(view.id)
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
      await api.edit(view.id, editIx, {
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
        <!-- Names what it does. "Undo" used to restore the model's reading, so taking back
             a confirmation also silently threw away the corrections that earned it. -->
        <button
          onclick={undoReview}
          disabled={busy}
          class="flex-1 rounded-2xl py-[13px] text-[13.5px] font-extrabold disabled:opacity-50"
          style="background:rgba(236,171,130,0.18);color:var(--accent)">Not checked after all</button
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

    <!-- One row per subject, each editing itself. The previous version made this a
         "pick one" row feeding a separate editor below, which read as a question about
         who was present and then asked you to set who "A person" is. A subject is not a
         mode: it is a thing in the picture, so its own row carries its own controls.

         Always shown, checked or not. Gating it on an unsettled moment made a frame with
         more than one subject uneditable: naming or removing the first also settled the
         moment, so the rows for the rest disappeared under the owner's hands. -->
    <div class="mt-3 text-[11px] font-bold tracking-wide uppercase" style="color:rgba(255,246,236,0.5)">
      Who's in this one?
    </div>
    <div class="mt-2 flex flex-col gap-[6px]">
      {#each view.subjects as s (s.ix)}
        <div class="rounded-[14px]" style="background:rgba(255,246,236,0.07)">
          <div class="flex items-center gap-[8px] px-[12px] py-[9px]">
            <span class="min-w-0 flex-1 truncate text-[13.5px] font-bold" style="color:#f4ece3">{s.label}</span>
            <!-- A name the model chose, not one you did. Marked here rather than left to
                 look settled, because this row is where saying otherwise costs one tap. -->
            {#if s.byModel}
              <span class="flex-shrink-0 rounded-full px-[8px] py-[2px] text-[10px] font-bold whitespace-nowrap" style={chipStyle('info')}>my guess</span>
            {/if}
            <button
              onclick={() => (editingIx = editingIx === s.ix ? null : s.ix)}
              disabled={busy}
              class="flex-shrink-0 text-[12px] font-bold"
              style="border:none;background:none;color:var(--accent)">{editingIx === s.ix ? 'Done' : 'Change'}</button>
            <button
              onclick={() => dropSubject(s.ix)}
              disabled={busy}
              title="Not actually there"
              class="flex-shrink-0 text-[15px]"
              style="border:none;background:none;color:rgba(255,246,236,0.5)">&times;</button>
          </div>
          {#if s.byModel && s.petId}
            <!-- The one-tap ending for a guess, addressed to THIS row. It corrects the one
                 sighting rather than passing a verdict on the frame: a two-dog frame carries
                 a separate guess per dog, and agreeing with one of them is not agreeing with
                 the other, nor a reason to close the panel before the owner reaches it. -->
            <div class="px-[12px] pb-[10px]">
              <button onclick={() => keepGuess(s.ix, s.petId!, s.label)} disabled={busy} class="w-full rounded-xl py-[8px] text-[12.5px] font-extrabold disabled:opacity-50" style="border:none;background:rgba(163,192,143,0.18);color:var(--good)">Yes, that's {s.label}</button>
            </div>
          {/if}
          {#if editingIx === s.ix}
            <!-- Options for THIS subject. What it currently is never appears as a choice. -->
            <div class="flex flex-wrap gap-[7px] px-[12px] pt-[2px] pb-[11px]">
              {#each pets.filter((p) => p.petId !== s.petId) as p (p.petId)}
                <button onclick={() => reclass(s.ix, p.petId, p.petName)} disabled={busy} class="rounded-full px-[12px] py-[6px] text-[12.5px] font-bold disabled:opacity-50" style="background:rgba(255,246,236,0.12);color:#f4ece3">It's {p.petName}</button>
              {/each}
              {#if !s.person}
                <button onclick={() => asNotMine(s.ix)} disabled={busy} class="rounded-full px-[12px] py-[6px] text-[12.5px] font-bold disabled:opacity-50" style="background:rgba(255,246,236,0.12);color:#f4ece3">Not my pet</button>
              {/if}
              {#if !s.person}
                <button onclick={() => asPerson(s.ix)} disabled={busy} class="rounded-full px-[12px] py-[6px] text-[12.5px] font-bold disabled:opacity-50" style="background:rgba(255,246,236,0.12);color:#f4ece3">A person</button>
              {/if}
            </div>
          {/if}
        </div>
      {/each}

      {#if addingSubject}
        <div class="flex flex-wrap items-center gap-[7px] rounded-[14px] px-[12px] py-[10px]" style="background:rgba(255,246,236,0.07)">
          <span class="text-[12.5px] font-bold" style="color:rgba(255,246,236,0.6)">Who else?</span>
          <!-- "Another" when one is already listed above. Two people in a frame is real, so
               the repeat is offered; it just has to read as a second one rather than as a
               fresh choice, which is how the same subject got added twice by accident. -->
          <button onclick={() => addSubject({ kind: 'person' })} disabled={busy} class="rounded-full px-[12px] py-[6px] text-[12.5px] font-bold disabled:opacity-40" style="background:rgba(255,246,236,0.12);color:#f4ece3">{hasPerson ? 'Another person' : 'A person'}</button>
          {#each speciesChoices as sp (sp)}
            <button onclick={() => addSubject({ kind: 'species', species: sp })} disabled={busy} class="rounded-full px-[12px] py-[6px] text-[12.5px] font-bold lowercase disabled:opacity-40" style="background:rgba(255,246,236,0.12);color:#f4ece3">{hasSpecies(sp) ? `Another ${sp}` : `A ${sp}`}</button>
          {/each}
          <button onclick={() => (addingSubject = false)} disabled={busy} class="text-[12px] font-bold" style="border:none;background:none;color:rgba(255,246,236,0.55)">Cancel</button>
        </div>
      {:else}
        <button onclick={() => (addingSubject = true)} disabled={busy} class="rounded-[14px] py-[9px] text-[12.5px] font-bold disabled:opacity-50" style="background:none;color:rgba(255,246,236,0.7);border:1px dashed rgba(255,246,236,0.22)">+ Someone else was here</button>
      {/if}
    </div>

    <!-- The wider undo, kept apart from taking back a verdict and never the default: it
         throws away every subject the owner named here. Always offered, because a moment
         corrected in an earlier session is exactly the one an owner comes back to undo, and
         gating it on edits made since this screen opened put that out of reach entirely. -->
    <button
      onclick={startOver}
      disabled={busy}
      class="mt-3 w-full rounded-2xl py-[11px] text-[12.5px] font-bold disabled:opacity-50"
      style="background:none;color:rgba(255,246,236,0.55);border:1px solid rgba(255,246,236,0.16)"
      >Start over from what I first saw</button
    >

    {#if view.media.kind !== 'audio'}
      <button
        onclick={toggleEdit}
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
            <div class="mb-[10px] text-[11px] leading-[1.4]" style="color:rgba(255,246,236,0.5)">Activity and behaviours apply to {editSubjectName}. The note and wellbeing cover the whole moment.</div>
          {/if}
          <button onclick={saveEdit} disabled={busy} class="w-full rounded-2xl py-[12px] text-[13.5px] font-extrabold disabled:opacity-50" style="background:var(--accent);color:var(--ink)">Save &amp; confirm</button>
          <div class="mt-[7px] text-center text-[11px]" style="color:rgba(255,246,236,0.45)">Saving also marks this moment as checked.</div>
        </div>
      {/if}
    {/if}
    </div>
  </div>
</div>
