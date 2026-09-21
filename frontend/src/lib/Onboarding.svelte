<script lang="ts">
  import { api, petPhotoUrl, type Pet, type Profile, type Status, type CameraRoom } from './api'
  import Avatar from './Avatar.svelte'
  import Paw from './Paw.svelte'
  import Toggle from './Toggle.svelte'
  import EmphasisToggles from './EmphasisToggles.svelte'
  import HouseholdFields from './HouseholdFields.svelte'
  import { avatarGradient, connDotStyle, connTextColor, friendlyError, initial as initialLetter, isPresent, uid } from './ui'
  import { EMPHASIS, DEFAULT_EMPHASIS, topicsToEmphasis, emphasisToTopics } from './emphasis'

  // An `initial` profile means editing an existing one, opened from the gear. Absent means
  // first-run setup.
  let {
    initial = null,
    onDone,
    onExit,
    ondirty,
    back = $bindable(() => {}),
  }: {
    initial?: Profile | null
    onDone: (p: Profile) => void
    // Called when Back is pressed at the first step while editing: close to Settings.
    onExit?: () => void
    ondirty?: (dirty: boolean) => void
    // Published so the app's phone-Back can step this wizard instead of nuking it.
    back?: () => void
  } = $props()

  const speciesList = [
    ['cat', 'Cat'],
    ['dog', 'Dog'],
    ['rabbit', 'Rabbit'],
    ['bird', 'Bird'],
    ['small', 'Small pet'],
    ['reptile', 'Reptile'],
    ['fish', 'Fish'],
    ['other', 'Other'],
  ]

  // One-time, non-reactive snapshot of the incoming profile. The component re-mounts when
  // editing starts, so the prop is stable for its lifetime.
  function readSeed(): Profile | null {
    return initial ? structuredClone($state.snapshot(initial)) : null
  }
  const seed = readSeed()

  function initEmphasis(): Record<string, boolean> {
    if (seed) return topicsToEmphasis(seed.report.topics)
    return Object.fromEntries(EMPHASIS.map((g) => [g.key, DEFAULT_EMPHASIS.has(g.key)]))
  }

  let step = $state(seed ? 1 : 0)
  let saving = $state(false)
  let saveError = $state('')

  // pets
  let added = $state<Pet[]>(seed ? seed.pets : [])
  let name = $state('')
  let species = $state('cat')
  let customSpecies = $state('')
  let description = $state('')
  let caveat = $state('')
  // The petId currently being edited, or null while adding a new pet. Editing in place
  // keeps the id, so keepsakes that reference this pet stay linked.
  let editingId = $state<string | null>(null)

  // --- pet photo (avatar) ----------------------------------------------------
  // The wizard defers all persistence, so freshly attached photos and clears are held here
  // keyed by petId, then applied in reconcileRoster at finish(). Values are data URLs; the
  // raw base64 is derived on send.
  let pendingPhotos = $state<Record<string, string>>({})
  let pendingRemovals = $state<Record<string, boolean>>({})
  // Form-local photo state for the pet currently being added/edited.
  let photoData = $state<string | null>(null) // a data URL just attached, or null
  let photoCleared = $state(false) // an existing photo was removed in this form
  let describing = $state(false)
  let describeError = $state('')
  let fileInput = $state<HTMLInputElement | null>(null)

  // prefs
  let emphasis = $state<Record<string, boolean>>(initEmphasis())
  let catFlap = $state(seed?.household.catFlap ?? false)
  let neighbourCat = $state(seed?.household.neighbourCat ?? false)
  let feedTimes = $state(seed?.household.feedTimes ?? '')

  // connect
  let cameras = $state<CameraRoom[]>(seed ? seed.cameras : [])
  let frigateUrl = $state(seed?.frigateUrl ?? '')
  let modelUrl = $state(seed?.modelUrl ?? '')
  let visionModel = $state(seed?.visionModel ?? '')
  let status = $state<Status | null>(null)
  let seeded = $state(false)

  // Dirtiness vs the incoming profile, so the app can confirm before a back/leave
  // discards edits. Mirrors Settings' normalized-snapshot approach.
  function snap(): string {
    return JSON.stringify({
      pets: added.map((p) => ({
        petId: p.petId,
        petName: p.petName.trim(),
        petSpecies: p.petSpecies,
        petDescription: p.petDescription.trim(),
        petNotes: p.petNotes ?? null,
        petArchivedAt: p.petArchivedAt,
      })),
      topics: [...emphasisToTopics(emphasis)].sort(),
      catFlap,
      neighbourCat,
      feedTimes: feedTimes.trim(),
      cameras: cameras.map((c) => ({ camId: c.camId, room: c.room.trim(), enabled: c.enabled })),
      frigateUrl: frigateUrl.trim(),
      modelUrl: modelUrl.trim(),
      visionModel: visionModel.trim(),
      // A pending photo attach or clear is an unsaved edit too, so leaving prompts.
      photos: Object.keys(pendingPhotos).sort(),
      photosRemoved: Object.keys(pendingRemovals).sort(),
    })
  }
  const baseline = snap()
  $effect(() => ondirty?.(snap() !== baseline))

  const titleize = (n: string) => n.replace(/_/g, ' ').replace(/\b\w/g, (c) => c.toUpperCase())

  function speciesValue(): string {
    if (species === 'other') return customSpecies.trim() || 'pet'
    return species
  }
  function resetForm() {
    editingId = null
    name = ''
    description = ''
    caveat = ''
    species = 'cat'
    customSpecies = ''
    photoData = null
    photoCleared = false
    describeError = ''
  }
  // A copy of a keyed map with one key dropped, so committing a photo can clear a
  // stale remove (and vice versa) without a leftover key.
  function without<T>(o: Record<string, T>, k: string): Record<string, T> {
    return Object.fromEntries(Object.entries(o).filter(([x]) => x !== k))
  }
  function savePet() {
    const n = name.trim()
    if (!n) return
    const petId = editingId ?? uid()
    const fields = {
      petName: n,
      petSpecies: speciesValue(),
      petDescription: description.trim(),
      petNotes: caveat.trim() || null,
    }
    if (editingId) added = added.map((p) => (p.petId === editingId ? { ...p, ...fields } : p))
    else added = [...added, { petId, ...fields, petArchivedAt: null, petPhoto: null }]
    // Record this pet's photo intent for finish(): a freshly attached photo, or a
    // clear, or neither (leave the saved one untouched).
    if (photoData) {
      pendingPhotos = { ...pendingPhotos, [petId]: photoData }
      pendingRemovals = without(pendingRemovals, petId)
    } else if (photoCleared) {
      pendingRemovals = { ...pendingRemovals, [petId]: true }
      pendingPhotos = without(pendingPhotos, petId)
    }
    resetForm()
  }
  // Only active pets are shown and edited here. Archived ones stay in `added`, and in the
  // saved profile, so their data is kept; they are managed from Settings.
  let livePets = $derived(added.filter(isPresent))
  let archivedCount = $derived(added.length - livePets.length)
  // A nudge rather than a block, for when the pet being entered could be mixed up with
  // another: the same kind of animal, or the very same name.
  let dupNudge = $derived.by(() => {
    const n = name.trim().toLowerCase()
    if (!n) return ''
    const sp = speciesValue().toLowerCase()
    const others = livePets.filter((p) => p.petId !== editingId)
    if (others.some((p) => p.petSpecies.toLowerCase() === sp && p.petName.trim().toLowerCase() === n))
      return `You already have a pet named ${name.trim()}. Give each a distinct name and a detailed description so they don't get mixed up.`
    if (others.some((p) => p.petSpecies.toLowerCase() === sp))
      return 'You have more than one pet that could look alike. A clear, detailed description of each helps me tell them apart.'
    return ''
  })
  function startEdit(p: Pet) {
    editingId = p.petId
    name = p.petName
    const known = speciesList.some(([k]) => k === p.petSpecies)
    species = known ? p.petSpecies : 'other'
    customSpecies = known ? '' : p.petSpecies
    description = p.petDescription
    caveat = p.petNotes ?? ''
    // The form's avatar derives from this pet's saved/pending photo; clear any
    // in-progress attach from a prior edit.
    photoData = null
    photoCleared = false
    describeError = ''
  }

  // The pet the form is editing (null while adding), so the avatar can fall back to
  // its saved photo when nothing new is attached.
  let editingPet = $derived(editingId ? (added.find((p) => p.petId === editingId) ?? null) : null)
  // What the form's avatar shows, in order: a freshly attached photo, then a pending or
  // saved photo of the pet being edited, then nothing, which falls back to the gradient. A
  // clear in this form beats all of them.
  let formPhoto = $derived.by(() => {
    if (photoData) return photoData
    if (photoCleared) return null
    if (!editingPet) return null
    return pendingPhotos[editingPet.petId] ?? petPhotoUrl(editingPet.petId, editingPet.petPhoto)
  })

  function pickPhoto() {
    fileInput?.click()
  }
  async function onPhotoPicked(e: Event) {
    const input = e.target as HTMLInputElement
    const file = input.files?.[0]
    input.value = '' // let re-picking the same file fire onchange again
    if (!file) return
    try {
      photoData = await downscaleToJpeg(file, 1024, 0.82)
      photoCleared = false
      describeError = ''
    } catch {
      describeError = 'That image could not be read. Try another.'
    }
  }
  function clearPhoto() {
    photoData = null
    photoCleared = true
    describeError = ''
  }
  // Draw the picked image onto a canvas capped at `max` px on its long edge and re-encode
  // as JPEG, keeping the upload small and handing the vision model a sane size. Returns a
  // data URL.
  function downscaleToJpeg(file: File, max: number, quality: number): Promise<string> {
    return new Promise((resolve, reject) => {
      const url = URL.createObjectURL(file)
      const img = new Image()
      img.onload = () => {
        URL.revokeObjectURL(url)
        const scale = Math.min(1, max / Math.max(img.width, img.height))
        const w = Math.max(1, Math.round(img.width * scale))
        const h = Math.max(1, Math.round(img.height * scale))
        const canvas = document.createElement('canvas')
        canvas.width = w
        canvas.height = h
        const ctx = canvas.getContext('2d')
        if (!ctx) return reject(new Error('no 2d context'))
        ctx.drawImage(img, 0, 0, w, h)
        resolve(canvas.toDataURL('image/jpeg', quality))
      }
      img.onerror = () => {
        URL.revokeObjectURL(url)
        reject(new Error('decode failed'))
      }
      img.src = url
    })
  }
  // Ask the vision model to draft (or enhance) the description from the attached
  // photo, dropping the result into the editable box.
  async function generateDescription() {
    if (!photoData) return
    describing = true
    describeError = ''
    try {
      const b64 = photoData.split(',')[1] ?? ''
      const r = await api.describePet({
        photo: b64,
        species: speciesValue(),
        description: description.trim() || undefined,
      })
      description = r.description
    } catch (e) {
      describeError = friendlyError(e)
    } finally {
      describing = false
    }
  }
  let confirmRemoveId = $state<string | null>(null)
  // Removing a pet archives it (soft): it is hidden and stops being identified,
  // but nothing is deleted and it can be restored (or permanently deleted) in
  // Settings, Manage data.
  function archivePet(id: string) {
    added = added.map((p) => (p.petId === id ? { ...p, petArchivedAt: new Date().toISOString() } : p))
    confirmRemoveId = null
    if (id === editingId) resetForm()
  }

  const camOnline = (n: string) => status?.cameras.find((c) => c.name === n)?.online ?? false

  async function pollStatus() {
    try {
      status = await api.status()
    } catch {
      status = null
    }
  }

  // Pull cameras from Frigate (no names/URLs to type). Merges in any not already
  // listed and never clobbers the owner's room/enabled edits, so a newly-added
  // Frigate camera simply appears on the next open.
  async function findCameras() {
    try {
      const ds = await api.cameras()
      const have = new Set(cameras.map((c) => c.camId))
      const add = ds
        .filter((d) => !have.has(d.name))
        .map((d) => ({ camId: d.name, room: titleize(d.name), enabled: true }))
      if (add.length) cameras = [...cameras, ...add]
    } catch {
      // Ignore; the empty state guides the owner to connect Frigate first.
    }
  }

  // Seed the Connect step from the current effective config the first time it opens.
  $effect(() => {
    if (step === 3 && !seeded) {
      seeded = true
      api
        .status()
        .then((s) => {
          status = s
          if (!frigateUrl.trim()) frigateUrl = s.frigate.url
          if (!modelUrl.trim()) modelUrl = s.model.url
        })
        .catch(() => {})
      // Cameras belong to Frigate: always merge in whatever it reports. The owner
      // enables/disables the ones they want; there is nothing to add or delete.
      findCameras()
    }
  })
  // Poll live reachability while the Connect step is showing.
  $effect(() => {
    if (step !== 3) return
    const id = setInterval(pollStatus, 4000)
    return () => clearInterval(id)
  })

  // The roster persists only through the per-pet endpoints (a settings save ignores
  // it, D7), so reconcile the wizard's pets against the server: add new ones, edit
  // changed fields, and flip archive state. The wizard only archives (never
  // hard-deletes), so a pet is never removed here.
  async function reconcileRoster(wizardPets: Pet[], serverPets: Pet[]) {
    const byId = new Map(serverPets.map((p) => [p.petId, p]))
    for (const a of wizardPets) {
      const existing = byId.get(a.petId)
      // A freshly attached photo (raw base64) or an explicit clear for this pet.
      const photo = pendingPhotos[a.petId]?.split(',')[1]
      const photoRemove = pendingRemovals[a.petId] === true
      if (!existing) {
        await api.addPet({
          id: a.petId,
          name: a.petName,
          species: a.petSpecies,
          description: a.petDescription,
          notes: a.petNotes,
          photo,
        })
        if (a.petArchivedAt) await api.archivePet(a.petId)
      } else {
        const textChanged =
          existing.petName !== a.petName ||
          existing.petSpecies !== a.petSpecies ||
          existing.petDescription !== a.petDescription ||
          existing.petNotes !== a.petNotes
        if (textChanged || photo || photoRemove) {
          await api.editPet(a.petId, {
            name: a.petName,
            species: a.petSpecies,
            description: a.petDescription,
            notes: a.petNotes,
            photo,
            photoRemove: photoRemove || undefined,
          })
        }
        const wasArchived = existing.petArchivedAt !== null
        const nowArchived = a.petArchivedAt !== null
        if (!wasArchived && nowArchived) await api.archivePet(a.petId)
        else if (wasArchived && !nowArchived) await api.unarchivePet(a.petId)
      }
    }
  }

  async function finish() {
    saving = true
    saveError = ''
    const topics = emphasisToTopics(emphasis)
    const wizardPets: Pet[] = $state.snapshot(added)
    const profile: Profile = {
      pets: wizardPets,
      report: { topics, freeform: seed?.report.freeform ?? null },
      household: {
        catFlap,
        neighbourCat,
        feedTimes: feedTimes.trim() || null,
        notes: seed?.household.notes ?? null,
      },
      cameras: $state.snapshot(cameras).filter((c) => c.camId.trim()),
      frigateUrl: frigateUrl.trim() || null,
      modelUrl: modelUrl.trim() || null,
      visionModel: visionModel.trim() || null,
      // Auto-detect the browser's zone on first setup; keep the saved one on edit.
      timeZone: seed?.timeZone ?? (Intl.DateTimeFormat().resolvedOptions().timeZone || null),
      gcWindowDays: seed?.gcWindowDays ?? 30,
      // Not asked for during setup; null follows the server's interval, and it is
      // adjustable in Settings afterwards.
      captureSecs: seed?.captureSecs ?? null,
      configuredAt: seed?.configuredAt ?? null,
    }
    try {
      // Reconcile against the LIVE roster (not the mount-time seed), so a retry after a
      // partial failure sees already-created pets as existing and edits them, rather than
      // re-adding (which the backend rejects as a 409, wedging the wizard).
      const serverPets = (await api.settings()).pets
      await reconcileRoster(wizardPets, serverPets)
      const saved = await api.saveSettings(profile)
      onDone(saved)
    } catch (e) {
      saveError = friendlyError(e)
    } finally {
      saving = false
    }
  }

  const next = () => (step = Math.min(4, step + 1))
  // The first real step of this flow: first-run starts at the welcome (0), editing
  // starts at step 1 (no welcome). Backing past the floor exits the wizard when
  // editing (returns to Settings) instead of dropping into the first-run welcome.
  const floor = seed ? 1 : 0
  function stepBack() {
    if (step > floor) step = step - 1
    else if (seed) onExit?.()
  }
  back = stepBack // publish for the app's phone-Back
  let stepLabel = $derived(step >= 1 && step <= 3 ? `Step ${step} of 3` : '')
</script>

{#snippet dots()}
  <div class="mb-[20px] flex items-center gap-[10px]">
    <div class="flex gap-[6px]">
      {#each [1, 2, 3] as n (n)}
        <span
          class="h-[8px] rounded-full"
          style="width:{n === step ? 22 : 8}px;background:{n <= step ? 'var(--accent)' : 'var(--line)'}"
        ></span>
      {/each}
    </div>
    <span class="text-[11px] font-extrabold tracking-wide" style="color:var(--faint)">{stepLabel}</span>
  </div>
{/snippet}

{#snippet navRow(nextLabel: string, nextFn: () => void, disabled = false)}
  <div class="mt-[20px] flex gap-[10px]">
    <button onclick={stepBack} class="flex-shrink-0 rounded-full border px-[22px] py-[14px] text-[15px] font-bold" style="border-color:var(--line);background:transparent;color:var(--muted)">Back</button>
    <button onclick={nextFn} disabled={disabled} class="font-head flex-1 rounded-full py-[14px] text-[16px] font-semibold disabled:opacity-60" style="background:var(--accent);color:var(--ink)">{nextLabel}</button>
  </div>
{/snippet}

<div class="fade flex min-h-screen flex-col px-[20px] pt-[20px] pb-[30px]">
  {#if step === 0}
    <div class="relative flex flex-1 flex-col items-center justify-center gap-[8px] text-center">
      <div class="pointer-events-none absolute top-[6%] left-1/2 h-[240px] w-[240px] -translate-x-1/2 rounded-full" style="background:radial-gradient(circle,var(--glow,rgba(236,171,130,0.13)),transparent 70%)"></div>
      <div class="relative flex h-[88px] w-[88px] items-center justify-center rounded-[28px]" style="background:var(--accent);color:var(--ink);box-shadow:0 16px 34px -8px var(--accent);animation:petUp .7s ease both"><Paw size={52} /></div>
      <div class="font-head mt-[18px] text-[31px] leading-[1.1] font-semibold" style="color:var(--text)">Welcome home</div>
      <div class="max-w-[290px] text-[15px] leading-[1.55]" style="color:var(--muted)">pet-report keeps a gentle eye on your animals and tells you how their day went, like a loving note from a sitter.</div>
      <div class="mt-[14px] inline-flex items-center gap-[7px] rounded-full border px-[14px] py-[7px] text-[12px] font-bold" style="background:var(--surface);border-color:var(--line);color:var(--muted)"><span style="color:var(--good)">⌂</span> Private, everything stays on your own hardware</div>
      <button onclick={() => (step = 1)} class="font-head mt-[26px] rounded-full px-[38px] py-[15px] text-[17px] font-semibold" style="background:var(--accent);color:var(--ink);box-shadow:0 10px 24px -6px var(--accent)">Let's set up</button>
      <button onclick={finish} disabled={saving} class="mt-[10px] bg-transparent text-[13px] font-bold" style="border:none;color:var(--faint)">Skip for now</button>
    </div>
  {:else if step === 1}
    <div class="fade">
      {@render dots()}
      <div class="font-head text-[24px] font-semibold" style="color:var(--text)">Who lives here?</div>
      <div class="mt-[4px] mb-[18px] text-[13.5px]" style="color:var(--muted)">A short note helps me tell everyone apart.</div>

      {#if livePets.length}
        <div class="mb-[18px] flex flex-col gap-[9px]">
          {#each livePets as p (p.petId)}
            <div class="rounded-[18px] border p-[11px]" style="background:var(--surface);border-color:{editingId === p.petId ? 'var(--accent)' : 'var(--line)'}">
              <div class="flex items-center gap-[12px]">
                <button onclick={() => startEdit(p)} class="flex min-w-0 flex-1 items-center gap-[12px] bg-transparent text-left" style="border:none;padding:0;color:inherit" title="Edit {p.petName}">
                  <Avatar name={p.petName} species={p.petSpecies} size={48} photo={pendingPhotos[p.petId] ?? petPhotoUrl(p.petId, p.petPhoto)} />
                  <div class="min-w-0 flex-1">
                    <div class="flex items-center gap-[7px]"><span class="font-head text-[16px] font-semibold" style="color:var(--text)">{p.petName}</span><span class="text-[10.5px] font-bold" style="color:var(--faint)">✎ edit</span></div>
                    <div class="truncate text-[12px]" style="color:var(--muted)"><span class="capitalize">{p.petSpecies}</span> · {p.petDescription || 'a new friend'}</div>
                    {#if p.petNotes}
                      <div class="mt-[4px] flex items-center gap-[5px] text-[11.5px]" style="color:var(--good)"><span>♥</span>{p.petNotes}</div>
                    {/if}
                  </div>
                </button>
                <button onclick={() => (confirmRemoveId = p.petId)} title="Archive" aria-label="Archive {p.petName}" class="flex h-[30px] w-[30px] flex-shrink-0 items-center justify-center rounded-full text-[15px]" style="border:none;background:var(--surface2);color:var(--faint)">×</button>
              </div>
              {#if confirmRemoveId === p.petId}
                <div class="fade mt-[10px] border-t pt-[10px]" style="border-color:var(--line)">
                  <div class="mb-[8px] text-[12px] leading-[1.45]" style="color:var(--muted)">Archive {p.petName}? Their moments and memories stay saved. You can bring them back, or delete for good, in Settings, Manage data.</div>
                  <div class="flex gap-[7px]">
                    <button onclick={() => archivePet(p.petId)} class="flex-1 rounded-[12px] py-[9px] text-[12.5px] font-extrabold" style="border:none;background:rgba(236,177,99,0.18);color:var(--watch)">Archive</button>
                    <button onclick={() => (confirmRemoveId = null)} class="flex-1 rounded-[12px] py-[9px] text-[12.5px] font-bold" style="border:none;background:var(--surface2);color:var(--muted)">Cancel</button>
                  </div>
                </div>
              {/if}
            </div>
          {/each}
        </div>
      {/if}

      {#if archivedCount > 0}
        <div class="mb-[16px] rounded-[14px] border border-dashed px-[13px] py-[10px] text-[12px] leading-[1.4]" style="background:var(--surface);border-color:var(--line);color:var(--muted)">{archivedCount} archived {archivedCount === 1 ? 'pet' : 'pets'}. Restore or remove for good in Settings, Manage data.</div>
      {/if}

      <div class="rounded-[20px] border border-dashed p-[16px]" style="background:var(--surface);border-color:var(--line)">
        <div class="mb-[14px] flex items-center gap-[12px]">
          <button type="button" onclick={pickPhoto} class="relative flex-shrink-0" title="Add a photo" aria-label="Add a photo">
            {#if formPhoto}
              <img src={formPhoto} alt="" class="h-[46px] w-[46px] rounded-full object-cover" style="box-shadow:0 4px 12px rgba(0,0,0,0.28)" />
            {:else}
              <div class="font-head flex h-[46px] w-[46px] items-center justify-center rounded-full font-semibold text-white" style="font-size:19px;background:{avatarGradient(speciesValue())};box-shadow:0 4px 12px rgba(0,0,0,0.28)">
                {name.trim() ? initialLetter(name) : ''}{#if !name.trim()}<Paw size={22} />{/if}
              </div>
            {/if}
            <span class="absolute -right-[2px] -bottom-[2px] flex h-[18px] w-[18px] items-center justify-center rounded-full text-[12px] leading-none" style="background:var(--accent);color:var(--ink);border:2px solid var(--surface)">+</span>
          </button>
          <div class="min-w-0 flex-1">
            <div class="font-head text-[15px] font-semibold whitespace-nowrap" style="color:var(--text)">{editingId ? 'Edit pet' : 'Add another'}</div>
            <div class="mt-[3px] flex gap-[12px] text-[12px] font-bold">
              <button type="button" onclick={pickPhoto} style="color:var(--accent)">{formPhoto ? 'Change photo' : 'Add photo'}</button>
              {#if formPhoto}<button type="button" onclick={clearPhoto} style="color:var(--faint)">Remove</button>{/if}
            </div>
          </div>
        </div>
        <input bind:this={fileInput} onchange={onPhotoPicked} type="file" accept="image/*" class="hidden" />
        <input bind:value={name} placeholder="Name" class="mb-[10px] w-full rounded-[14px] border px-[15px] py-[12px] text-[14px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
        <div class="mb-[10px] flex flex-wrap gap-[7px]">
          {#each speciesList as [k, l] (k)}
            <button onclick={() => (species = k)} class="rounded-full border px-[16px] py-[8px] text-[13.5px] font-bold" style={species === k ? 'background:var(--accent);color:var(--ink);border-color:transparent' : 'background:transparent;border-color:var(--line);color:var(--muted)'}>{l}</button>
          {/each}
        </div>
        {#if species === 'other'}
          <input bind:value={customSpecies} placeholder="What kind of animal? e.g. ferret, tortoise, parrot" class="fade mb-[10px] w-full rounded-[14px] border px-[15px] py-[12px] text-[13.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
        {/if}
        <textarea bind:value={description} placeholder="Describe them so I can spot them: size, shape, markings, colour..." class="mb-[6px] h-[62px] w-full resize-none rounded-[14px] border px-[15px] py-[12px] text-[13.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)"></textarea>
        <div class="mb-[10px] px-[2px] text-[11px] leading-[1.4]" style="color:var(--faint)">Tip: if your cameras use night vision (black &amp; white), include traits that aren't colour: size, shape, ears, tail, gait. It helps me tell your pets apart.</div>
        <button
          type="button"
          onclick={generateDescription}
          disabled={!photoData || describing}
          class="mb-[8px] flex w-full items-center justify-center gap-[8px] rounded-[14px] border py-[11px] text-[13px] font-extrabold disabled:opacity-40"
          style="border-color:var(--accent);background:transparent;color:var(--accent)"
        >
          {#if describing}
            <span class="flex gap-[4px]">
              <span class="h-[6px] w-[6px] rounded-full" style="background:var(--accent);animation:petThink 1.2s ease infinite"></span>
              <span class="h-[6px] w-[6px] rounded-full" style="background:var(--accent);animation:petThink 1.2s ease .2s infinite"></span>
              <span class="h-[6px] w-[6px] rounded-full" style="background:var(--accent);animation:petThink 1.2s ease .4s infinite"></span>
            </span>
            Reading the photo...
          {:else}
            ✨ {description.trim() ? 'Enhance with photo' : 'Generate description from photo'}
          {/if}
        </button>
        {#if describeError}
          <div class="mb-[8px] px-[2px] text-[12px] leading-[1.4]" style="color:#e26d5c">{describeError}</div>
        {:else if !photoData}
          <div class="mb-[8px] px-[2px] text-[11px] leading-[1.4]" style="color:var(--faint)">{formPhoto ? 'Re-attach the photo to regenerate the description from it.' : 'Add a photo and I can write the description for you.'}</div>
        {/if}
        <input bind:value={caveat} placeholder="Anything I shouldn't worry about? e.g. three legs are normal" class="mb-[12px] w-full rounded-[14px] border px-[15px] py-[12px] text-[13.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
        {#if dupNudge}
          <div class="fade mb-[12px] flex items-start gap-[8px] rounded-[14px] px-[13px] py-[11px]" style="background:rgba(236,177,99,0.12);border:1px solid rgba(236,177,99,0.3)">
            <span class="text-[14px]" style="color:var(--watch)">!</span>
            <span class="text-[12.5px] leading-[1.45]" style="color:var(--text)">{dupNudge}</span>
          </div>
        {/if}
        <div class="flex gap-[8px]">
          {#if editingId}
            <button onclick={resetForm} class="flex-shrink-0 rounded-[14px] border px-[18px] py-[12px] text-[14px] font-bold" style="border-color:var(--line);background:transparent;color:var(--muted)">Cancel</button>
          {/if}
          <button onclick={savePet} disabled={!name.trim()} class="flex-1 rounded-[14px] border py-[12px] text-[14px] font-extrabold disabled:opacity-40" style="border-color:var(--accent);background:{editingId ? 'var(--accent)' : 'transparent'};color:{editingId ? 'var(--ink)' : 'var(--accent)'}">{editingId ? 'Save changes' : '+ Add pet'}</button>
        </div>
      </div>

      {@render navRow('Continue', next)}
    </div>
  {:else if step === 2}
    <div class="fade">
      {@render dots()}
      <div class="font-head text-[24px] font-semibold" style="color:var(--text)">What matters most?</div>
      <div class="mt-[4px] mb-[18px] text-[13.5px]" style="color:var(--muted)">I'll lean into these in your daily report.</div>

      <div class="mb-[22px]"><EmphasisToggles bind:emphasis /></div>

      <div class="font-head mx-[2px] mb-[3px] text-[16px] font-semibold" style="color:var(--text)">About your home</div>
      <div class="mx-[2px] mb-[12px] text-[12.5px]" style="color:var(--muted)">A little context helps me read what I see, so I don't mistake the neighbour's cat for yours, or worry about a normal cat-flap trip.</div>
      <HouseholdFields bind:catFlap bind:neighbourCat bind:feedTimes />

      {@render navRow('Continue', next)}
    </div>
  {:else if step === 3}
    <div class="fade">
      {@render dots()}
      <div class="font-head text-[24px] font-semibold" style="color:var(--text)">Where should I look?</div>
      <div class="mt-[4px] mb-[12px] text-[13.5px]" style="color:var(--muted)">Point me at your cameras and the services that watch them. It all runs on your own hardware.</div>
      <div class="mb-[16px] rounded-[14px] border border-dashed px-[13px] py-[10px] text-[12px] leading-[1.45]" style="border-color:var(--line);color:var(--muted)">Heads-up: the daily stats (meals, litter, naps) only reflect what a camera can see. Point them where your pets eat, rest, and pass through for the fullest picture.</div>

      <div class="mx-[2px] mb-[10px] flex items-center gap-[8px]">
        <span class="font-head text-[15px] font-semibold whitespace-nowrap" style="color:var(--text)">Cameras</span>
        {#if cameras.length}<span class="text-[11px] font-bold whitespace-nowrap" style="color:var(--muted)">· {cameras.filter((c) => c.enabled).length} of {cameras.length} on</span>{/if}
      </div>
      {#if cameras.length === 0}
        <div class="mb-[20px] rounded-[16px] border border-dashed p-[16px] text-center text-[12.5px] leading-[1.5]" style="background:var(--surface);border-color:var(--line);color:var(--muted)">
          Your cameras appear here on their own once Frigate is connected below, no feed URLs to type. Give it a moment after you save.
        </div>
      {:else}
        <div class="mb-[20px] flex flex-col gap-[9px]">
          {#each cameras as c (c.camId)}
            <div class="flex items-center gap-[10px] rounded-[16px] border p-[12px]" style="background:var(--surface);border-color:var(--line)">
              <span class="h-[8px] w-[8px] flex-shrink-0 rounded-full" style="background:{camOnline(c.camId) ? '#e26d5c' : 'var(--faint)'};box-shadow:{camOnline(c.camId) ? '0 0 8px #e26d5c' : 'none'}"></span>
              <div class="min-w-0 flex-1">
                <input bind:value={c.room} placeholder="Room name" class="font-head w-full bg-transparent text-[14.5px] font-semibold outline-none" style="border:none;color:var(--text)" />
                <div class="mt-[1px] truncate font-mono text-[10.5px]" style="color:var(--faint)">{c.camId} · from Frigate</div>
              </div>
              <Toggle checked={c.enabled} onToggle={() => (c.enabled = !c.enabled)} label={c.room || c.camId} />
            </div>
          {/each}
        </div>
      {/if}

      <div class="font-head mx-[2px] mb-[10px] text-[15px] font-semibold" style="color:var(--text)">Services</div>
      <div class="mb-[10px] rounded-[18px] border p-[15px]" style="background:var(--surface);border-color:var(--line)">
        <div class="mb-[3px] flex items-center justify-between gap-[8px]">
          <span class="text-[13.5px] font-extrabold" style="color:var(--text)">Frigate</span>
          <span class="flex items-center gap-[5px]"><span class="h-[8px] w-[8px] rounded-full" style={connDotStyle(!!status?.frigate.reachable)}></span><span class="text-[11px] font-extrabold" style="color:{connTextColor(!!status?.frigate.reachable)}">{status?.frigate.reachable ? 'reachable' : 'not reachable'}</span></span>
        </div>
        <div class="mb-[10px] text-[11.5px]" style="color:var(--muted)">The service that spots motion and clips.</div>
        <input bind:value={frigateUrl} placeholder="http://...:5000" class="w-full rounded-[11px] border px-[12px] py-[10px] font-mono text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
      </div>
      <div class="rounded-[18px] border p-[15px]" style="background:var(--surface);border-color:var(--line)">
        <div class="mb-[3px] flex items-center justify-between gap-[8px]">
          <span class="text-[13.5px] font-extrabold" style="color:var(--text)">Vision model</span>
          <span class="flex items-center gap-[5px]"><span class="h-[8px] w-[8px] rounded-full" style={connDotStyle(!!status?.model.reachable)}></span><span class="text-[11px] font-extrabold" style="color:{connTextColor(!!status?.model.reachable)}">{status?.model.reachable ? 'reachable' : 'not reachable'}</span></span>
        </div>
        <div class="mb-[10px] text-[11.5px]" style="color:var(--muted)">The local AI that watches clips and writes the notes.</div>
        <input bind:value={modelUrl} placeholder="http://...:11434" class="w-full rounded-[11px] border px-[12px] py-[10px] font-mono text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
        <input bind:value={visionModel} placeholder="model name (optional), exactly as your server lists it" class="mt-[8px] w-full rounded-[11px] border px-[12px] py-[10px] font-mono text-[12px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--muted)" />
      </div>
      <div class="mt-[14px] flex items-center gap-[7px] text-[11.5px]" style="color:var(--muted)"><span style="color:var(--good)">⌂</span> These addresses stay on your network. Status updates a moment after you save.</div>

      {@render navRow('Continue', next)}
    </div>
  {:else}
    <div class="flex flex-1 flex-col items-center justify-center gap-[10px] text-center">
      <div style="color:var(--good);animation:petUp .6s ease both"><Paw size={74} /></div>
      <div class="font-head mt-[6px] text-[27px] font-semibold" style="color:var(--text)">All set 🐾</div>
      <div class="max-w-[270px] text-[15px] leading-[1.55]" style="color:var(--muted)">I'll start keeping a gentle eye on {added.map((p) => p.petName).join(' & ') || 'your pets'}. You'll get a soft update each morning and evening.</div>
      <button onclick={finish} disabled={saving} class="font-head mt-[20px] rounded-full px-[34px] py-[15px] text-[17px] font-semibold disabled:opacity-60" style="background:var(--accent);color:var(--ink);box-shadow:0 10px 24px -6px var(--accent)">{saving ? 'Saving...' : 'Open Today'}</button>
      {#if saveError}<div class="mt-[12px] max-w-[280px] text-[13px]" style="color:#e26d5c">{saveError}</div>{/if}
    </div>
  {/if}
</div>
