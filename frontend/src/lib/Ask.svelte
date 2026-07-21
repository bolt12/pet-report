<script lang="ts">
  import { api, type ObsView, type Pet, type ReviewPreset } from './api'
  import { openLightbox } from './lightbox.svelte'
  import Paw from './Paw.svelte'
  import Thumb from './Thumb.svelte'
  import GearButton from './GearButton.svelte'
  import TimeText from './TimeText.svelte'
  import { fmtTime, friendlyError, ymdForOffset, ymdOf } from './ui'

  let { pets, onnav }: { pets: Pet[]; onnav: (s: string, arg?: string | ReviewPreset) => void } = $props()

  type Status = 'idle' | 'thinking' | 'answered'
  let status = $state<Status>('idle')
  let q = $state('')
  let asked = $state('')
  let answer = $state('')
  let refs = $state<ObsView[]>([])

  // Ask spans multiple days, so a written time should link to the day the answer
  // is about, not today. Use the day of the moments the agent actually looked at
  // (they share a day when the question was about one day); fall back to today.
  let answerDate = $derived(refs.length ? ymdOf(refs[0].at) : ymdForOffset(0))

  const suggestions = [
    { t: 'When was everyone last seen?', hint: 'last seen' },
    { t: 'Anything I should worry about?', hint: 'wellbeing' },
    { t: 'What happened in the last couple of hours?', hint: 'activity' },
  ]

  let canSend = $derived(q.trim().length > 0)

  // The agent answers in one request (it calls read-only tools server-side before
  // replying), so we show one honest "looking" state, not a fabricated per-step
  // checklist. Real streamed steps are a later, backend-supported upgrade.
  async function run(text: string) {
    const question = text.trim()
    if (!question) return
    asked = question
    status = 'thinking'
    answer = ''
    refs = []
    try {
      const r = await api.ask(question)
      answer = r.answer
      refs = r.refs
    } catch (e) {
      answer = friendlyError(e)
    } finally {
      status = 'answered'
    }
  }

  function reset() {
    status = 'idle'
    q = ''
    answer = ''
    refs = []
  }
  const openRef = (o: ObsView) => openLightbox(refs, o.id, pets, () => {})
</script>

<div class="fade flex min-h-full flex-col px-[18px] pt-[14px] pb-[120px] lg:mx-auto lg:max-w-[780px] lg:pt-[28px] lg:pb-[60px]">
  <div class="flex items-center justify-between gap-[10px]">
    <div class="font-head text-[22px] font-semibold" style="color:var(--text)">Ask about your pets</div>
    <span class="lg:hidden"><GearButton {onnav} /></span>
  </div>
  <div class="mb-4 text-[13.5px]" style="color:var(--muted)">Ask about any day. I'll look back through your pets' moments and tell you what I find.</div>

  <div class="mb-[14px] flex gap-[8px]">
    <input bind:value={q} onkeydown={(e) => e.key === 'Enter' && run(q)} placeholder="e.g. Where is everyone resting?" class="min-w-0 flex-1 rounded-full border px-[18px] py-[13px] text-[14px] outline-none" style="background:var(--surface);border-color:var(--line);color:var(--text)" />
    <button onclick={() => run(q)} disabled={!canSend} class="flex h-[48px] w-[48px] flex-shrink-0 items-center justify-center rounded-full text-[19px] disabled:opacity-40" style="background:var(--accent);color:var(--ink);border:none" aria-label="Ask">→</button>
  </div>

  {#if status === 'idle'}
    <div>
      <div class="mb-[22px] flex items-center gap-[12px] rounded-[20px] border p-[15px_16px]" style="background:linear-gradient(140deg,var(--surface),var(--surface2));border-color:var(--line)">
        <div class="flex h-[44px] w-[44px] flex-shrink-0 items-center justify-center rounded-[14px]" style="background:var(--accent);color:var(--ink)"><Paw size={26} /></div>
        <div class="text-[13.5px] leading-[1.5]" style="color:var(--text);opacity:.92">Ask me anything about your pets, on any day, and I'll look back through their moments for you.</div>
      </div>
      <div class="mx-[2px] mb-[10px] text-[11.5px] font-extrabold tracking-wide uppercase" style="color:var(--muted)">Try asking</div>
      <div class="flex flex-col gap-[9px]">
        {#each suggestions as s (s.t)}
          <button onclick={() => run(s.t)} class="flex w-full items-center gap-[12px] rounded-[16px] border p-[14px_15px] text-left" style="background:var(--surface);border-color:var(--line);color:inherit">
            <span class="flex flex-shrink-0" style="color:var(--accent)"><Paw size={17} /></span>
            <div class="min-w-0 flex-1"><div class="font-head text-[14.5px] font-medium" style="color:var(--text)">{s.t}</div><div class="mt-[1px] text-[11px] font-bold capitalize" style="color:var(--faint)">{s.hint}</div></div>
            <span class="flex-shrink-0 text-[18px]" style="color:var(--faint)">›</span>
          </button>
        {/each}
      </div>
    </div>
  {:else if status === 'thinking'}
    <div class="rounded-[22px] border p-[18px]" style="background:linear-gradient(140deg,var(--surface),var(--surface2));border-color:var(--line)">
      <div class="flex items-center gap-[10px]">
        <div class="flex h-[34px] w-[34px] flex-shrink-0 items-center justify-center rounded-[11px]" style="background:var(--accent);color:var(--ink)"><Paw size={20} /></div>
        <span class="font-head text-[15px] font-semibold" style="color:var(--text)">Looking back</span>
        <span class="ml-auto flex gap-[4px]"><span class="h-[7px] w-[7px] rounded-full" style="background:var(--accent);animation:petThink 1.2s ease infinite"></span><span class="h-[7px] w-[7px] rounded-full" style="background:var(--accent);animation:petThink 1.2s ease .2s infinite"></span><span class="h-[7px] w-[7px] rounded-full" style="background:var(--accent);animation:petThink 1.2s ease .4s infinite"></span></span>
      </div>
      <div class="mt-[13px] text-[13px] leading-[1.5]" style="color:var(--muted)">Reading back through your pets' moments and stats to answer. This can take a moment.</div>
    </div>
  {:else}
    <div class="fade">
      <div class="relative overflow-hidden rounded-[22px] border p-[18px]" style="background:linear-gradient(140deg,var(--surface),var(--surface2));border-color:var(--line)">
        <div class="absolute -right-[10px] -bottom-[14px]" style="color:var(--accent);opacity:.1"><Paw size={80} /></div>
        <div class="mb-[8px] text-[12px] font-extrabold tracking-wide uppercase" style="color:var(--accent)">You asked</div>
        <div class="font-head mb-[16px] text-[16px] font-medium" style="color:var(--text)">{asked}</div>
        <div class="mb-[11px] flex items-center gap-[9px]"><span class="flex h-[28px] w-[28px] flex-shrink-0 items-center justify-center rounded-[9px]" style="background:var(--accent);color:var(--ink)"><Paw size={16} /></span><span class="text-[11.5px] font-extrabold tracking-wide uppercase" style="color:var(--muted)">Here's what I found</span></div>
        <div class="text-[14.5px] leading-[1.6]" style="color:var(--text);opacity:.94"><TimeText text={answer} date={answerDate} {onnav} /></div>
      </div>

      {#if refs.length}
        <div class="mt-[16px] mb-[10px] text-[12px] font-extrabold tracking-wide uppercase" style="color:var(--muted)">Moments I looked at</div>
        <div class="flex gap-[10px] overflow-x-auto pb-[4px]" data-scroll>
          {#each refs as o (o.id)}
            <button onclick={() => openRef(o)} class="flex-shrink-0 rounded-2xl border p-[8px] text-left" style="width:132px;background:var(--surface);border-color:var(--line);color:inherit">
              <div class="relative mb-[7px] h-[78px] overflow-hidden rounded-xl"><Thumb img={o.media.stillUrl} media={o.media.kind} room={o.room} pawSize={26} /></div>
              <div class="text-[11.5px] font-extrabold" style="color:var(--text)">{fmtTime(o.at)} · {o.room}</div>
              <div class="mt-[2px] text-[11px]" style="color:var(--muted)">{o.subjectLabel}</div>
            </button>
          {/each}
        </div>
      {/if}

      <button onclick={reset} class="mt-[16px] w-full rounded-full border py-[11px] text-[13.5px] font-extrabold" style="background:transparent;border-color:var(--line);color:var(--accent)">Ask something else</button>
    </div>
  {/if}
</div>
