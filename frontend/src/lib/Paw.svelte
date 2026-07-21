<script lang="ts">
  let { size = 24 }: { size?: number } = $props()
</script>

<svg viewBox="0 0 24 24" width={size} height={size} fill="currentColor" aria-hidden="true">
  <ellipse cx="12" cy="15.5" rx="5.4" ry="4.2" />
  <circle cx="6" cy="9.2" r="2.3" />
  <circle cx="10.8" cy="6.8" r="2.5" />
  <circle cx="16" cy="8" r="2.4" />
  <circle cx="19.4" cy="12.2" r="2" />
</svg>
