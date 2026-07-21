<script lang="ts">
  // The small wellbeing status dot (colour keyed off a moment's wellbeing), shared
  // by Today, Moments, and the lightbox so the colour mapping and shape can't drift.
  // Size is a prop so each call site keeps its own diameter (7px in lists, 9px in
  // the lightbox); it renders via inline style, so any px value works without a
  // Tailwind class per size.
  import { WELLBEING_DOT } from './ui'

  let { wellbeing, size = 7 }: { wellbeing: string; size?: number } = $props()
</script>

<span
  class="flex-shrink-0 rounded-full"
  style="width:{size}px;height:{size}px;background:{WELLBEING_DOT[wellbeing] ?? 'var(--faint)'}"
></span>
