<script lang="ts">
  import { untrack } from 'svelte'
  import { api, petPhotoUrl, type Profile, type Status, type StatusEndpoint } from './api'
  import Avatar from './Avatar.svelte'
  import Toggle from './Toggle.svelte'
  import EmphasisToggles from './EmphasisToggles.svelte'
  import HouseholdFields from './HouseholdFields.svelte'
  import BackButton from './BackButton.svelte'
  import { theme, toggleTheme, toggleMotion } from './theme.svelte'
  import { connDotStyle, connTextColor, friendlyError, isPresent } from './ui'
  import { topicsToEmphasis, emphasisToTopics } from './emphasis'

  let {
    profile,
    onnav,
    onProfile,
    onclose,
    ondirty,
  }: {
    profile: Profile
    onnav: (s: string, arg?: string) => void
    onProfile: (p: Profile) => void
    onclose: () => void
    ondirty?: (dirty: boolean) => void
  } = $props()

  // Only the pets currently present; archived pets live in Manage data.
  let activePets = $derived(profile.pets.filter(isPresent))

  // --- editable config, seeded once from a plain snapshot of the incoming
  // profile (a snapshot, so seeding local $state does not tie it to the prop;
  // read inside a function so it is a one-time capture, not a live reference). --
  function readSeed(): Profile {
    return $state.snapshot(profile) as Profile
  }
  const seed = readSeed()
  let frigateUrl = $state(seed.frigateUrl ?? '')
  let modelUrl = $state(seed.modelUrl ?? '')
  let visionModel = $state(seed.visionModel ?? '')
  let timeZone = $state(seed.timeZone ?? '')
  let catFlap = $state(seed.household.catFlap)
  let neighbourCat = $state(seed.household.neighbourCat)
  let feedTimes = $state(seed.household.feedTimes ?? '')
  let emphasis = $state<Record<string, boolean>>(topicsToEmphasis(seed.report.topics))
  let cameras = $state(seed.cameras.map((c) => ({ ...c })))
  let gcWindowDays = $state(seed.gcWindowDays ?? 30)
  // The whole-number, >= 1 value actually stored, so dirty-tracking, the save payload,
  // and (after save) the field itself all agree even if the input holds a blank or a
  // fractional number.
  // >= 1 whole number. An empty field (null) defaults to 30; 0 or a fraction clamps up
  // to the 1-day minimum (so entering 0 is a real change, not silently 30).
  const gcDays = () => Math.max(1, Math.round(gcWindowDays ?? 30))
  // The currently-stored retention. Shortening the window below it deletes un-kept
  // moments, so a Save that does is gated behind a confirmation (below).
  let savedGc = $state(seed.gcWindowDays ?? 30)
  // Set when a pending Save would delete moments: holds the count to confirm first.
  let confirmPurge = $state<{ effectiveDays: number; count: number } | null>(null)
  // Editing the window while the confirmation is open makes its count stale (it was
  // computed for the old value), so drop back to the plain Save and re-confirm. Track
  // only gcWindowDays; untrack the reset so setting confirmPurge does not clear itself.
  $effect(() => {
    gcWindowDays
    untrack(() => {
      if (confirmPurge) confirmPurge = null
    })
  })

  // The cleanup schedule and a manual "run now", so the owner sees when it happens and
  // can force it. The batch (which runs the cleanup) is a loop of the serve process.
  let sched = $state<{ batchHours: number[]; nextRunAt: string | null } | null>(null)
  let cleaning = $state(false)
  let cleanMsg = $state('')
  async function loadSchedule() {
    try {
      sched = await api.cleanupInfo()
    } catch {
      sched = null
    }
  }
  loadSchedule()
  const fmtHours = (hs: number[]) =>
    hs.map((h) => `${h}:00`).join(hs.length === 2 ? ' and ' : ', ')
  // Time until a FUTURE instant (relTime only does the past).
  function untilText(iso: string): string {
    const mins = Math.round((new Date(iso).getTime() - Date.now()) / 60000)
    if (mins < 1) return 'shortly'
    if (mins < 60) return `in ${mins} min`
    const hrs = Math.round(mins / 60)
    return `in about ${hrs} hour${hrs === 1 ? '' : 's'}`
  }
  async function runCleanup() {
    if (cleaning) return
    cleaning = true
    cleanMsg = ''
    try {
      const r = await api.cleanupNow()
      cleanMsg = r.busy
        ? 'A cleanup is already running.'
        : r.collected > 0
          ? `Removed ${r.collected} moment${r.collected === 1 ? '' : 's'}.`
          : 'Nothing to clean up right now.'
      loadSchedule()
    } catch (e) {
      cleanMsg = friendlyError(e)
    } finally {
      cleaning = false
    }
  }

  // The IANA zones the browser knows, for the timezone picker (a free-text input
  // is the fallback where the API is unavailable).
  const zones: string[] = (() => {
    const f = (Intl as { supportedValuesOf?: (k: string) => string[] }).supportedValuesOf
    try {
      return f ? f('timeZone') : []
    } catch {
      return []
    }
  })()
  const deviceZone = (() => {
    try {
      return Intl.DateTimeFormat().resolvedOptions().timeZone
    } catch {
      return ''
    }
  })()

  // Dirty tracking via a normalized snapshot, so the Save bar shows only on a real
  // change and clears itself after a successful save.
  const snapshot = () =>
    JSON.stringify({
      frigateUrl: frigateUrl.trim(),
      modelUrl: modelUrl.trim(),
      visionModel: visionModel.trim(),
      timeZone: timeZone.trim(),
      catFlap,
      neighbourCat,
      feedTimes: feedTimes.trim(),
      topics: [...emphasisToTopics(emphasis)].sort(),
      cameras: cameras.map((c) => ({ camId: c.camId, room: c.room.trim(), enabled: c.enabled })),
      gcWindowDays: gcDays(),
    })
  let savedSnap = $state(snapshot())
  let dirty = $derived(snapshot() !== savedSnap)
  // Publish dirtiness so the app can confirm before a back/leave discards edits.
  $effect(() => ondirty?.(dirty))

  let saving = $state(false)
  let saveError = $state('')

  // The Save action. If it would shorten the retention below the stored value AND
  // that deletes un-kept moments, it does NOT save yet: it asks how many, so the owner
  // confirms (or keeps some first). Any other change saves straight through.
  async function save() {
    if (saving || confirmPurge) return
    if (gcDays() < savedGc) {
      try {
        const r = await api.cleanupPreview(gcDays())
        if (r.count > 0) {
          confirmPurge = { effectiveDays: r.effectiveDays, count: r.count }
          return
        }
      } catch {
        // A failed preview must not block saving; fall through to persist.
      }
    }
    await doSave()
  }

  async function doSave() {
    if (saving) return
    saving = true
    saveError = ''
    const updated: Profile = {
      ...profile,
      report: { ...profile.report, topics: emphasisToTopics(emphasis) },
      household: { ...profile.household, catFlap, neighbourCat, feedTimes: feedTimes.trim() || null },
      cameras: cameras.map((c) => ({ ...c, room: c.room.trim() || c.camId })),
      frigateUrl: frigateUrl.trim() || null,
      modelUrl: modelUrl.trim() || null,
      visionModel: visionModel.trim() || null,
      timeZone: timeZone.trim() || null,
      gcWindowDays: gcDays(),
    }
    try {
      const p = await api.saveSettings(updated)
      onProfile(p)
      // Snap the field to the whole-number value stored, and reset the confirmation
      // baseline to the new value.
      gcWindowDays = gcDays()
      savedGc = gcDays()
      confirmPurge = null
      savedSnap = snapshot()
      // Re-test with the freshly-saved URLs so the reachability dots reflect them.
      refreshStatus()
    } catch (e) {
      saveError = friendlyError(e)
    } finally {
      saving = false
    }
  }

  // --- live reachability test (tests the saved URLs) -------------------------
  let status = $state<Status | null>(null)
  let testing = $state<'frigate' | 'model' | null>(null)
  async function refreshStatus(which: 'frigate' | 'model' | null = null) {
    testing = which
    try {
      status = await api.status()
    } catch {
      status = null
    } finally {
      testing = null
    }
  }
  refreshStatus()

  const themes: { k: 'night' | 'day'; l: string }[] = [
    { k: 'night', l: 'Night' },
    { k: 'day', l: 'Light' },
  ]

  let conns = $derived([
    { key: 'frigate' as const, label: 'Frigate', sub: 'Spots motion and saves clips', ep: status?.frigate },
    { key: 'model' as const, label: 'Vision model', sub: 'Watches clips, writes the notes', ep: status?.model },
  ])
  const connLabel = (which: 'frigate' | 'model', ep?: StatusEndpoint) =>
    testing === which ? 'Checking…' : !ep ? 'not tested' : ep.reachable ? 'reachable' : 'not reachable'
</script>

<div class="fade px-[18px] pt-[14px] pb-[150px] lg:mx-auto lg:max-w-[720px] lg:px-[24px] lg:pt-[24px]">
  <div class="mb-[18px] flex items-center gap-[12px]">
    <BackButton onclick={onclose} />
    <div class="font-head text-[22px] font-semibold" style="color:var(--text)">Settings</div>
  </div>

  <!-- appearance -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Appearance</div>
  <div class="mb-[20px] rounded-[20px] border p-[16px]" style="background:var(--surface);border-color:var(--line)">
    <div class="mb-[13px] flex gap-[5px] rounded-[13px] p-[4px]" style="background:var(--surface2)">
      {#each themes as t (t.k)}
        <button onclick={() => theme.name !== t.k && toggleTheme()} class="font-head flex-1 rounded-[10px] py-[10px] text-[14px] font-semibold" style={theme.name === t.k ? 'background:var(--accent);color:var(--ink)' : 'background:transparent;color:var(--muted)'}>{t.l}</button>
      {/each}
    </div>
    <div class="flex items-center justify-between gap-[12px]">
      <div><div class="text-[14px] font-bold" style="color:var(--text)">Reduce motion</div><div class="mt-[1px] text-[11.5px]" style="color:var(--faint)">Calmer, minimal animation</div></div>
      <Toggle checked={theme.reduceMotion} onToggle={toggleMotion} label="Reduce motion" />
    </div>
  </div>

  <!-- pets -->
  <div class="mx-[2px] mb-[9px] flex items-center justify-between">
    <span class="text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Pets</span>
    <button onclick={() => onnav('setup')} class="bg-transparent text-[12.5px] font-extrabold" style="border:none;color:var(--accent)">Manage ›</button>
  </div>
  <div class="mb-[20px] flex flex-col gap-[9px]">
    {#each activePets as p (p.petId)}
      <button onclick={() => onnav('pets', p.petId)} class="tappable flex w-full items-center gap-[12px] rounded-[18px] border p-[11px] text-left" style="background:var(--surface);border-color:var(--line);color:inherit">
        <Avatar name={p.petName} species={p.petSpecies} size={44} photo={petPhotoUrl(p.petId, p.petPhoto)} />
        <div class="min-w-0 flex-1">
          <div class="font-head text-[15.5px] font-semibold" style="color:var(--text)">{p.petName}</div>
          <div class="truncate text-[12px]" style="color:var(--muted)"><span class="capitalize">{p.petSpecies}</span>{p.petDescription ? ` · ${p.petDescription}` : ''}</div>
          {#if p.petNotes}<div class="mt-[3px] flex items-center gap-[5px] text-[11.5px]" style="color:var(--good)"><span>♥</span>{p.petNotes}</div>{/if}
        </div>
        <span class="flex-shrink-0 text-[18px]" style="color:var(--faint)">›</span>
      </button>
    {/each}
    {#if activePets.length === 0}
      <div class="rounded-[16px] border border-dashed p-[14px] text-center text-[13px]" style="background:var(--surface);border-color:var(--line);color:var(--muted)">No pets yet. Tap Manage to add them.</div>
    {/if}
  </div>

  <!-- report emphasis -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">In your daily report</div>
  <div class="mb-[20px]"><EmphasisToggles bind:emphasis /></div>

  <!-- home context -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Your home</div>
  <div class="mb-[20px]"><HouseholdFields bind:catFlap bind:neighbourCat bind:feedTimes /></div>

  <!-- connections -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Connections</div>
  {#each conns as row (row.key)}
    <div class="mb-[10px] rounded-[18px] border p-[15px]" style="background:var(--surface);border-color:var(--line)">
      <div class="mb-[10px] flex items-center justify-between gap-[10px]">
        <div class="min-w-0">
          <div class="text-[13.5px] font-extrabold" style="color:var(--text)">{row.label}</div>
          <div class="text-[11.5px]" style="color:var(--muted)">{row.sub}</div>
        </div>
        <div class="flex flex-shrink-0 items-center gap-[10px]">
          <span class="flex items-center gap-[5px]">
            <span class="h-[8px] w-[8px] rounded-full" style={connDotStyle(!!row.ep?.reachable)}></span>
            <span class="text-[11px] font-extrabold whitespace-nowrap" style="color:{connTextColor(!!row.ep?.reachable)}">{connLabel(row.key, row.ep)}</span>
          </span>
          <button onclick={() => refreshStatus(row.key)} disabled={testing !== null} class="rounded-full border px-[14px] py-[7px] text-[12px] font-extrabold disabled:opacity-60" style="border-color:var(--line);background:var(--surface2);color:var(--accent)">{testing === row.key ? 'Testing…' : 'Test'}</button>
        </div>
      </div>
      {#if row.key === 'frigate'}
        <input bind:value={frigateUrl} placeholder="http://...:5000" class="w-full rounded-[11px] border px-[12px] py-[10px] font-mono text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
      {:else}
        <input bind:value={modelUrl} placeholder="http://...:11434" class="w-full rounded-[11px] border px-[12px] py-[10px] font-mono text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
        <input bind:value={visionModel} placeholder="model name (optional), exactly as your server lists it" class="mt-[8px] w-full rounded-[11px] border px-[12px] py-[10px] font-mono text-[12px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--muted)" />
      {/if}
    </div>
  {/each}
  <div class="mt-[2px] mb-[20px] text-[11px]" style="color:var(--faint)">Test checks the last saved address. Save your changes, then test.</div>

  <!-- timezone -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Timezone</div>
  <div class="mb-[20px] rounded-[18px] border p-[15px]" style="background:var(--surface);border-color:var(--line)">
    <div class="mb-[9px] text-[11.5px] leading-[1.45]" style="color:var(--muted)">Sets when a day starts and ends, and the morning/evening split.</div>
    {#if zones.length}
      <select bind:value={timeZone} class="w-full rounded-[12px] border px-[12px] py-[10px] text-[13.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)">
        <option value="">Use this device{deviceZone ? ` (${deviceZone})` : ''}</option>
        {#each zones as z (z)}
          <option value={z}>{z}</option>
        {/each}
      </select>
    {:else}
      <input bind:value={timeZone} placeholder={deviceZone || 'e.g. Europe/Lisbon'} class="w-full rounded-[12px] border px-[12px] py-[10px] font-mono text-[12.5px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
    {/if}
  </div>

  <!-- keeping moments -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">How long to keep moments</div>
  <div class="mb-[20px] rounded-[18px] border p-[15px]" style="background:var(--surface);border-color:var(--line)">
    <div class="mb-[12px] text-[13.5px] leading-[2.1]" style="color:var(--text)">
      Delete moments you haven't
      <button
        onclick={() => onnav('keepsakes')}
        class="font-bold underline"
        style="background:none;border:none;padding:0;color:var(--good);cursor:pointer">Kept</button>
      once they are
      <input
        type="number"
        min="1"
        bind:value={gcWindowDays}
        class="mx-[2px] w-[62px] rounded-[10px] border px-[10px] py-[6px] text-[13.5px] outline-none"
        style="background:var(--bg);border-color:var(--line);color:var(--text)" />
      days old.
    </div>
    <div class="text-[11.5px] leading-[1.55]" style="color:var(--muted)">
      Changing this never deletes anything on its own. It sets how long the everyday
      moments you haven't <span class="font-bold" style="color:var(--good)">Kept</span>
      are stored before a background cleanup removes them, to free up space. Your day
      summaries, pet stats, and Kept moments are always safe. If a shorter window would
      delete anything, Save asks you to confirm first.
    </div>
    <div class="mt-[12px] flex flex-wrap items-center gap-[10px] border-t pt-[12px]" style="border-color:var(--line)">
      <button
        onclick={runCleanup}
        disabled={cleaning}
        class="rounded-[12px] px-[14px] py-[8px] text-[12.5px] font-bold disabled:opacity-60"
        style="border:none;background:var(--surface2);color:var(--accent)">{cleaning ? 'Cleaning…' : 'Run cleanup now'}</button>
      <span class="text-[11.5px]" style="color:var(--muted)">
        {#if cleanMsg}{cleanMsg}
        {:else if sched}Runs automatically at {fmtHours(sched.batchHours)}{#if sched.nextRunAt} · next {untilText(sched.nextRunAt)}{/if}.
        {:else}Runs automatically with each update.{/if}
      </span>
    </div>
  </div>

  <!-- cameras -->
  {#if cameras.length}
    <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Cameras</div>
    <div class="mb-[20px] flex flex-col gap-[9px]">
      {#each cameras as c (c.camId)}
        {@const on = status?.cameras.find((s) => s.name === c.camId)?.online}
        <div class="flex items-center gap-[10px] rounded-[16px] border p-[12px]" style="background:var(--surface);border-color:var(--line)">
          <span class="h-[8px] w-[8px] flex-shrink-0 rounded-full" style={connDotStyle(!!on)} title={on ? 'online' : 'offline'}></span>
          <div class="min-w-0 flex-1">
            <input bind:value={c.room} placeholder="Room name" class="font-head w-full bg-transparent text-[14.5px] font-semibold outline-none" style="border:none;color:var(--text)" />
            <div class="mt-[1px] truncate font-mono text-[10.5px]" style="color:var(--faint)">{c.camId} · {on ? 'online' : 'offline'} · from Frigate</div>
          </div>
          <Toggle checked={c.enabled} onToggle={() => (c.enabled = !c.enabled)} label={c.room || c.camId} />
        </div>
      {/each}
    </div>
  {/if}

  <button onclick={() => onnav('setup')} class="mt-[2px] flex w-full items-center justify-between rounded-[16px] border py-[13px] pr-[15px] pl-[16px] text-[13.5px] font-bold" style="border-color:var(--line);background:var(--surface);color:var(--text)"><span>Re-run guided setup</span><span style="color:var(--faint)">›</span></button>
  <button onclick={() => onnav('managedata')} class="mt-[10px] flex w-full items-center justify-between rounded-[16px] border py-[13px] pr-[15px] pl-[16px] text-[13.5px] font-bold" style="border-color:var(--line);background:var(--surface);color:var(--text)"><span>Manage data</span><span style="color:var(--faint)">›</span></button>
  <div class="mt-[16px] flex items-center gap-[7px] text-[11.5px]" style="color:var(--muted)"><span style="color:var(--good)">⌂</span> Everything runs on your own hardware. Nothing leaves your network.</div>
</div>

<!-- sticky save bar: only while there are unsaved edits -->
{#if dirty || saveError}
  <div class="fixed right-0 bottom-[78px] left-0 z-20 mx-auto max-w-[480px] px-[18px] lg:bottom-0 lg:left-[236px] lg:max-w-[720px] lg:px-[24px] lg:pb-[20px]">
    {#if saveError}<div class="mb-[8px] rounded-[12px] px-[13px] py-[9px] text-[12.5px] font-bold" style="background:rgba(226,109,92,0.16);color:#e26d5c">{saveError}</div>{/if}
    {#if confirmPurge}
      <!-- Destructive-change confirmation: a shorter window lets the next cleanup delete moments. -->
      <div class="rounded-[16px] p-[14px]" style="background:var(--surface);border:1px solid rgba(226,109,92,0.4)">
        <div class="mb-[11px] text-[13px] leading-[1.5]" style="color:var(--text)">
          Nothing is deleted right now. With this window, the next background cleanup will
          remove about <b>{confirmPurge.count}</b>
          {confirmPurge.count === 1 ? 'moment' : 'moments'} you haven't
          <button onclick={() => onnav('keepsakes')} class="font-bold underline" style="background:none;border:none;padding:0;color:var(--good);cursor:pointer">Kept</button>
          (older than {confirmPurge.effectiveDays} {confirmPurge.effectiveDays === 1 ? 'day' : 'days'}). Kept moments always stay; once cleanup runs it can't be undone.
        </div>
        <div class="flex gap-[8px]">
          <button onclick={() => (confirmPurge = null)} class="flex-1 rounded-[14px] py-[12px] text-[13.5px] font-bold" style="border:none;background:var(--surface2);color:var(--muted)">Cancel</button>
          <button onclick={doSave} disabled={saving} class="flex-1 rounded-[14px] py-[12px] text-[13.5px] font-extrabold disabled:opacity-60" style="border:none;background:rgba(226,109,92,0.9);color:#fff">{saving ? 'Saving…' : 'Save this window'}</button>
        </div>
      </div>
    {:else if dirty}
      <button onclick={save} disabled={saving} class="font-head w-full rounded-[16px] py-[14px] text-[15px] font-semibold disabled:opacity-60" style="background:var(--accent);color:var(--ink);box-shadow:0 10px 26px -6px var(--accent)">{saving ? 'Saving…' : 'Save changes'}</button>
    {/if}
  </div>
{/if}
