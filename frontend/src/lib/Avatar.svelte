<script lang="ts">
  import { avatarGradient, initial } from './ui'

  let {
    name,
    species,
    size = 54,
    ring = false,
    photo = null,
  }: {
    name: string
    species: string
    size?: number
    ring?: boolean
    // A photo URL to show instead of the gradient-and-initial fallback, or null.
    photo?: string | null
  } = $props()

  // If the photo URL 404s (e.g. a stale token), fall back to the gradient. Reset the
  // flag whenever the source changes so a fresh photo gets a fresh try.
  let broken = $state(false)
  $effect(() => {
    photo
    broken = false
  })

  const frame = (): string =>
    `width:${size}px;height:${size}px;box-shadow:0 4px 12px rgba(0,0,0,0.28);${ring ? 'border:2.5px solid var(--accent)' : ''}`
</script>

{#if photo && !broken}
  <img
    src={photo}
    alt={name}
    class="flex-shrink-0 rounded-full object-cover"
    style={frame()}
    onerror={() => (broken = true)}
  />
{:else}
  <div
    class="font-head flex flex-shrink-0 items-center justify-center rounded-full font-semibold text-white"
    style="{frame()};font-size:{Math.round(size * 0.42)}px;background:{avatarGradient(species)}"
  >
    {initial(name)}
  </div>
{/if}
