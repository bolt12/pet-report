<script lang="ts">
  let {
    checked,
    onToggle,
    label = '',
  }: { checked: boolean; onToggle: () => void; label?: string } = $props()
</script>

<button
  onclick={onToggle}
  class="relative h-[26px] w-[44px] flex-shrink-0 rounded-full border"
  style="background:{checked ? 'var(--accent)' : 'var(--surface2)'};border-color:var(--line)"
  aria-label={label}
  aria-pressed={checked}
>
  <span
    class="absolute top-[2px] h-[20px] w-[20px] rounded-full bg-white"
    style="left:{checked ? 20 : 2}px;box-shadow:0 1px 3px rgba(0,0,0,0.3);transition:left var(--anim)"
  ></span>
</button>
