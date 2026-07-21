<script lang="ts">
  import { api, type Profile, type ReviewPreset } from './lib/api'
  import Today from './lib/Today.svelte'
  import Pets from './lib/Pets.svelte'
  import Ask from './lib/Ask.svelte'
  import Review from './lib/Review.svelte'
  import Settings from './lib/Settings.svelte'
  import ManageData from './lib/ManageData.svelte'
  import Keepsakes from './lib/Keepsakes.svelte'
  import Onboarding from './lib/Onboarding.svelte'
  import Lightbox from './lib/Lightbox.svelte'
  import Paw from './lib/Paw.svelte'
  import NavIcon from './lib/NavIcon.svelte'
  import { theme } from './lib/theme.svelte'
  import { layout } from './lib/layout.svelte'
  import Sidebar from './lib/Sidebar.svelte'
  import { lb, closeLightbox, stepLightbox } from './lib/lightbox.svelte'
  import { live, closeLive } from './lib/liveview.svelte'

  type View = 'today' | 'pets' | 'ask' | 'review' | 'settings' | 'managedata' | 'keepsakes'
  let view = $state<View>('today')
  let profile = $state<Profile | null>(null)
  let loading = $state(true)
  let petFocus = $state<string | undefined>(undefined)
  // A filter preset carried to Moments when navigating there: either a quick key
  // ("needs a look", visitor sightings) or a full deep-link object from a stat card.
  let reviewPreset = $state<ReviewPreset>({})
  let configuring = $state(false)

  // Screens that track unsaved edits, so leaving can confirm before discarding.
  let settingsDirty = $state(false)
  let wizardDirty = $state(false)
  // A pending navigation held back by the discard confirm (null = no confirm shown).
  let pendingLeave = $state<null | (() => void)>(null)
  // The wizard publishes its own Back handler so the phone gesture steps through it
  // instead of nuking the whole flow.
  let wizardBack = $state<() => void>(() => {})

  async function load() {
    loading = true
    try {
      profile = await api.settings()
    } catch {
      profile = null
    }
    loading = false
  }
  load()

  let needsSetup = $derived(profile !== null && profile.configuredAt === null)

  // Whether the screen we would leave has unsaved edits worth confirming. Only the
  // actually-mounted screen counts: opening the wizard from Settings sets `configuring`
  // while `view` stays 'settings' and Settings unmounts without emitting ondirty(false),
  // so a stale `settingsDirty` must not leak in while the wizard governs.
  let dirtyNow = $derived(configuring ? wizardDirty : view === 'settings' && settingsDirty)
  // Run a navigation, but if the current screen is dirty, ask before discarding.
  function guarded(proceed: () => void) {
    if (dirtyNow) pendingLeave = proceed
    else proceed()
  }
  function confirmDiscard() {
    const go = pendingLeave
    pendingLeave = null
    go?.()
  }
  const cancelDiscard = () => (pendingLeave = null)

  // Navigation from child screens. 'setup' opens the wizard (pre-filled); 'review'
  // may carry a needs-a-look preset. Routed through 'guarded' so tabs, the gear, and
  // in-screen links all honour a dirty Settings/setup.
  function nav(s: string, arg?: string | ReviewPreset) {
    guarded(() => applyNav(s, arg))
  }
  function applyNav(s: string, arg?: string | ReviewPreset) {
    if (s === 'setup') {
      // Settings unmounts as the wizard opens and won't emit ondirty(false), so clear
      // its flag here; the wizard's own wizardDirty governs discard prompts from now on.
      settingsDirty = false
      configuring = true
      return
    }
    if (s === 'pets' && typeof arg === 'string') petFocus = arg
    if (s === 'keepsakes') {
      view = 'keepsakes'
      return
    }
    if (s === 'review') {
      // A string arg is a quick preset key; an object is a full deep-link preset.
      const keyed: Record<string, ReviewPreset> = { needs: { needs: true }, visitor: { who: ['visitor'] } }
      reviewPreset = typeof arg === 'object' ? arg : (keyed[arg ?? ''] ?? {})
    } else {
      reviewPreset = {}
    }
    view = s as View
  }

  // The parent each nested screen steps back to, so the phone Back gesture matches
  // what each screen's own back (‹) button does. Only the two screens that are not
  // children of Today need an entry; everything else defaults to Today.
  const parentOf: Partial<Record<View, View>> = { managedata: 'settings' }

  // --- Back button ("Home, then exit") ---------------------------------------
  // The app is a state-driven SPA, so the phone Back gesture has nothing to pop.
  // We keep a single "trap" history entry armed whenever we are not at the Today
  // root; a Back consumes it and steps one layer toward Today, re-arming until
  // Today is reached, after which the next Back leaves the app.
  let atRoot = $derived(
    loading ||
      (view === 'today' && lb.index < 0 && !live.open && !configuring && !needsSetup && profile !== null),
  )
  let armed = false
  function ensureArmed() {
    if (!atRoot && !armed) {
      history.pushState({ prTrap: true }, '')
      armed = true
    }
  }
  // Keep the trap in step with atRoot. Forward navigation or opening an overlay arms one.
  // A programmatic close or in-app nav that lands back at the root while a trap is still
  // armed leaves that trap stale on the stack, so pop it here; otherwise the next phone
  // Back gets eaten as a no-op instead of navigating. A native Back cannot reach here
  // armed, since onPop clears `armed` before the overlay or view closes, so only a
  // non-popstate close hits the atRoot && armed branch.
  $effect(() => {
    if (atRoot && armed) {
      armed = false
      history.back() // drop the stale trap; onPop then sees atRoot and exits
    } else {
      ensureArmed()
    }
  })
  // The single place the dismissal ladder lives, so no close path can drift.
  function goBack() {
    if (pendingLeave) return cancelDiscard() // dismiss the discard confirm first
    if (lb.index >= 0) return closeLightbox()
    if (live.open) return closeLive()
    if (configuring) return wizardBack() // step through the wizard; it exits at its floor
    if (needsSetup) return // forced first-run setup must not be backed out of
    if (view !== 'today') guarded(() => (view = parentOf[view] ?? 'today'))
  }
  function onPop() {
    armed = false // the browser popped our trap
    if (atRoot) return // on the base entry; the next native Back exits the app
    goBack()
    ensureArmed() // re-arm synchronously so a buffer always remains while non-root
  }

  const tabs: { key: View; label: string }[] = [
    { key: 'today', label: 'Today' },
    { key: 'pets', label: 'Pets' },
    { key: 'ask', label: 'Ask' },
    { key: 'review', label: 'Review' },
  ]
</script>

<svelte:window onpopstate={onPop} />

<div
  class="theme-{theme.name} {theme.reduceMotion ? 'reduce-motion' : ''} min-h-screen"
  style="background:var(--bg);color:var(--text)"
>
  {#if loading}
    <div class="flex min-h-screen items-center justify-center" style="color:var(--muted)">
      <span style="color:var(--accent);opacity:.5"><Paw size={40} /></span>
    </div>
  {:else if !profile || needsSetup || configuring}
    <div class="mx-auto max-w-[480px] lg:max-w-[560px]">
      <Onboarding
        initial={needsSetup ? null : profile}
        onDone={(p) => ((profile = p), (configuring = false), (view = 'today'))}
        onExit={() => guarded(() => (configuring = false))}
        ondirty={(d) => (wizardDirty = d)}
        bind:back={wizardBack}
      />
    </div>
  {:else}
    <!-- The screen switch, declared once and rendered into whichever shell the
         viewport calls for: the desktop sidebar layout or the mobile column. -->
    {#snippet screens()}
      <!-- profile is non-null in this branch (see the {:else if} guard above); the
           snippet closure does not carry that narrowing, so assert it here. -->
      {#if view === 'today'}
        <Today profile={profile!} onnav={nav} />
      {:else if view === 'pets'}
        <Pets initialPetId={petFocus} pets={profile!.pets} onnav={nav} />
      {:else if view === 'ask'}
        <Ask pets={profile!.pets} onnav={nav} />
      {:else if view === 'settings'}
        <Settings profile={profile!} onnav={nav} onProfile={(p) => (profile = p)} onclose={() => guarded(() => (view = 'today'))} ondirty={(d) => (settingsDirty = d)} />
      {:else if view === 'managedata'}
        <ManageData profile={profile!} onProfile={(p) => (profile = p)} onclose={() => (view = 'settings')} />
      {:else if view === 'keepsakes'}
        <Keepsakes pets={profile!.pets} onnav={nav} />
      {:else}
        <Review profile={profile!} preset={reviewPreset} onnav={nav} />
      {/if}
    {/snippet}

    {#if layout.isDesktop}
      <!-- Desktop shell: a fixed sidebar plus a single scrolling content region.
           Each screen centers itself at its own comfortable max-width. -->
      <div class="flex h-screen">
        <Sidebar {view} onnav={nav} />
        <main class="h-screen min-w-0 flex-1 overflow-y-auto" data-scroll>
          {@render screens()}
        </main>
      </div>
    {:else}
      <div class="mx-auto min-h-screen max-w-[480px] pb-[86px]">
        {@render screens()}
      </div>

      <nav
        class="fixed right-0 bottom-0 left-0 z-10 mx-auto flex max-w-[480px] items-center justify-around border-t px-[12px] pt-[10px] pb-[26px]"
        style="background:var(--bg);border-color:var(--line);box-shadow:0 -6px 20px rgba(0,0,0,0.22)"
      >
        {#each tabs as t (t.key)}
          <button
            onclick={() => nav(t.key)}
            class="flex flex-col items-center gap-[4px] px-[10px] py-[4px]"
            style="color:{view === t.key ? 'var(--accent)' : 'var(--faint)'}"
          >
            <span class="flex h-[22px] items-center"><NavIcon kind={t.key} pawSize={22} /></span>
            <span class="text-[10.5px] font-extrabold">{t.label}</span>
            <span class="h-[3px] w-[3px] rounded-full" style="background:{view === t.key ? 'var(--accent)' : 'transparent'}"></span>
          </button>
        {/each}
      </nav>
    {/if}
  {/if}

  {#if lb.index >= 0 && lb.list[lb.index]}
    <Lightbox
      obs={lb.list[lb.index]}
      pets={lb.pets}
      hasPrev={lb.index > 0}
      hasNext={lb.index < lb.list.length - 1}
      onclose={closeLightbox}
      onprev={() => stepLightbox(-1)}
      onnext={() => stepLightbox(1)}
      onchanged={() => lb.onChanged()}
    />
  {/if}

  {#if pendingLeave}
    <div class="fixed inset-0 z-30 flex items-center justify-center p-[24px]" style="background:rgba(0,0,0,0.5)">
      <div class="w-full max-w-[320px] rounded-[22px] border p-[20px]" style="background:var(--surface);border-color:var(--line)">
        <div class="font-head text-[17px] font-semibold" style="color:var(--text)">Discard changes?</div>
        <div class="mt-[6px] text-[13.5px] leading-[1.5]" style="color:var(--muted)">You have unsaved edits here. Leaving now will lose them.</div>
        <div class="mt-[18px] flex gap-[9px]">
          <button onclick={cancelDiscard} class="flex-1 rounded-[14px] border py-[12px] text-[13.5px] font-bold" style="border-color:var(--line);background:var(--surface2);color:var(--text)">Keep editing</button>
          <button onclick={confirmDiscard} class="flex-1 rounded-[14px] py-[12px] text-[13.5px] font-extrabold" style="border:none;background:#e26d5c;color:#fff">Discard</button>
        </div>
      </div>
    </div>
  {/if}
</div>
