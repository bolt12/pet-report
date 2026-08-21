<script lang="ts">
  import type { ReviewPreset, TimeOfDayValue } from './api'

  // Renders model-written prose with any clock time turned into a link that opens
  // Moments for `date`, narrowed to a small window around that time. XSS-safe: the
  // text is only ever emitted as text nodes and <button>s, never injected as HTML.
  let { text, date, onnav }: { text: string; date: string; onnav: (s: string, arg?: string | ReviewPreset) => void } = $props()

  // 12h ("8", "8:14", optional space, am/pm with optional dots) OR 24h ("08:14").
  // The 12h form is tried first so "8:14am" is one match, not "8:14" + stray.
  const RE = /\b(1[0-2]|0?[1-9])(?::([0-5]\d))?\s?([ap])\.?m\.?\b|\b([01]?\d|2[0-3]):([0-5]\d)\b/gi

  type Part = { s: string; min: number | null }
  function tokenize(t: string): Part[] {
    const out: Part[] = []
    let last = 0
    for (const m of t.matchAll(RE)) {
      const i = m.index ?? 0
      if (i > last) out.push({ s: t.slice(last, i), min: null })
      out.push({ s: m[0], min: minutesOf(m) })
      last = i + m[0].length
    }
    if (last < t.length) out.push({ s: t.slice(last), min: null })
    return out
  }

  function minutesOf(m: RegExpMatchArray): number {
    if (m[3]) {
      // 12h: hour (+ optional minutes) with am/pm.
      let h = Number(m[1])
      const min = m[2] ? Number(m[2]) : 0
      const pm = m[3].toLowerCase() === 'p'
      if (pm && h < 12) h += 12
      if (!pm && h === 12) h = 0
      return h * 60 + min
    }
    return Number(m[4]) * 60 + Number(m[5]) // 24h
  }

  // The server browse filters time-of-day in coarse buckets, so a clicked clock time
  // opens Moments for that day narrowed to its part of the day.
  //
  // These bounds restate the server's, which is a second copy of one rule in a second
  // language. They are pinned against each other by the wire-vocabulary test rather than
  // by anything the compiler can see, so keep the two in step.
  function bucketOfMinute(min: number): TimeOfDayValue {
    if (min >= 5 * 60 && min < 12 * 60) return 'morning'
    if (min >= 12 * 60 && min < 17 * 60) return 'afternoon'
    if (min >= 17 * 60 && min < 21 * 60) return 'evening'
    return 'night'
  }

  function jump(min: number) {
    onnav('review', { from: date, to: date, timeOfDay: bucketOfMinute(min) })
  }

  let parts = $derived(tokenize(text))
</script>

{#each parts as p, i (i)}{#if p.min !== null}<button onclick={() => jump(p.min as number)} style="border:none;background:none;padding:0;font:inherit;color:var(--accent);text-decoration:underline;text-underline-offset:2px;cursor:pointer">{p.s}</button>{:else}{p.s}{/if}{/each}
