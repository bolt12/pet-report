<script lang="ts">
  // A status chip pill (label tinted by kind), shared by Today, the lightbox, and
  // Pets so the pill shape and colour styling can't drift. Two size presets keep
  // the call sites pixel-identical: 'sm' (px-[9px] py-[3px] text-[11px], used in
  // Today and the lightbox) and 'md' (px-[10px] py-[4px] text-[11.5px], used for
  // Pets' at-a-glance chips). The class strings are literal so Tailwind's JIT sees
  // them. `nowrap` matches the lightbox's whitespace-nowrap.
  import { chipStyle } from './ui'

  let {
    label,
    kind,
    size = 'sm',
    nowrap = false,
  }: { label: string; kind: string; size?: 'sm' | 'md'; nowrap?: boolean } = $props()

  let sizeClass = $derived(size === 'md' ? 'px-[10px] py-[4px] text-[11.5px]' : 'px-[9px] py-[3px] text-[11px]')
</script>

<span class="rounded-full font-bold {sizeClass}{nowrap ? ' whitespace-nowrap' : ''}" style={chipStyle(kind)}>{label}</span>
