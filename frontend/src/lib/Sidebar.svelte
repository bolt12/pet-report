<script lang="ts">
  // The desktop left nav: replaces the mobile bottom tab bar at >= 1024px. It
  // carries the same four destinations (Today, Pets, Ask, Moments) with the same
  // glyphs the bottom bar draws, plus the theme toggle and Settings that live in
  // each screen's mobile header. Navigation goes through the app's guarded `onnav`
  // so a dirty Settings/setup still prompts before leaving.
  import Paw from './Paw.svelte'
  import NavIcon from './NavIcon.svelte'
  import { theme, toggleTheme } from './theme.svelte'

  let { view, onnav }: { view: string; onnav: (s: string) => void } = $props()

  const items: { key: string; label: string }[] = [
    { key: 'today', label: 'Today' },
    { key: 'pets', label: 'Pets' },
    { key: 'ask', label: 'Ask' },
    { key: 'review', label: 'Moments' },
  ]

  // A nav row: active gets the accent tint + a soft surface pill; the rest read as
  // muted until hovered (handled below by the shared class).
  const rowStyle = (on: boolean) =>
    on ? 'background:var(--surface2);color:var(--accent)' : 'background:transparent;color:var(--faint)'
</script>

<aside
  class="flex h-screen w-[236px] flex-shrink-0 flex-col border-r px-[16px] py-[22px]"
  style="background:var(--surface);border-color:var(--line)"
>
  <div class="mb-[24px] flex items-center gap-[10px] px-[8px] pt-[4px]">
    <span class="flex h-[38px] w-[38px] items-center justify-center rounded-[12px]" style="background:var(--accent);color:var(--ink)"><Paw size={22} /></span>
    <span class="font-head text-[19px] font-semibold whitespace-nowrap" style="color:var(--text)">pet-report</span>
  </div>

  <nav class="flex flex-col gap-[4px]">
    {#each items as it (it.key)}
      <button
        onclick={() => onnav(it.key)}
        class="side-row flex w-full items-center gap-[12px] rounded-[14px] px-[12px] py-[11px] text-left text-[14px] font-extrabold"
        style={rowStyle(view === it.key)}
      >
        <span class="flex w-[22px] justify-center">
          <NavIcon kind={it.key} pawSize={20} />
        </span>
        {it.label}
      </button>
    {/each}
  </nav>

  <div class="mt-auto flex flex-col gap-[4px] border-t pt-[12px]" style="border-color:var(--line)">
    <button
      onclick={toggleTheme}
      class="side-row flex w-full items-center gap-[12px] rounded-[14px] px-[12px] py-[11px] text-left text-[14px] font-extrabold"
      style={rowStyle(false)}
    >
      <span class="flex w-[22px] justify-center text-[16px]">{theme.name === 'night' ? '☾' : '☀'}</span>
      {theme.name === 'night' ? 'Night' : 'Light'}
    </button>
    <button
      onclick={() => onnav('settings')}
      class="side-row flex w-full items-center gap-[12px] rounded-[14px] px-[12px] py-[11px] text-left text-[14px] font-extrabold"
      style={rowStyle(view === 'settings' || view === 'managedata')}
    >
      <span class="flex w-[22px] justify-center text-[17px]">⚙</span>
      Settings
    </button>
  </div>
</aside>

<style>
  /* A gentle hover for the idle rows, matching the app's tappable affordance. */
  .side-row {
    transition:
      background 0.14s ease,
      color 0.14s ease;
  }
  .side-row:hover {
    color: var(--text);
    background: var(--surface2);
  }
</style>
