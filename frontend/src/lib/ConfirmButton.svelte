<script lang="ts">
  // The two-tap confirm-delete triad used in Manage data: an idle button that arms,
  // then a destructive "confirm" button beside a "cancel" button.
  // Controlled: the parent owns `armed`, so a screen can keep one confirmation open
  // at a time (Manage data's single `confirm` key) and couple it to a busy flag.
  // Every per-state class/style is a prop so each site keeps its exact appearance;
  // the wrapper element is a fragment (no layout box of its own).

  let {
    armed,
    onarm,
    onconfirm,
    oncancel,
    label,
    confirmLabel,
    cancelLabel = 'Cancel',
    disabled = false,
    idleDisabled,
    idleClass,
    idleStyle,
    confirmClass,
    confirmStyle,
    cancelClass,
    cancelStyle,
  }: {
    armed: boolean
    onarm: () => void
    onconfirm: () => void
    oncancel: () => void
    label: string
    confirmLabel: string
    cancelLabel?: string
    // Shared disabled for the confirm/cancel pair (e.g. a busy flag).
    disabled?: boolean
    // Optional separate disabled for the idle button (defaults to `disabled`).
    idleDisabled?: boolean
    idleClass: string
    idleStyle: string
    confirmClass: string
    confirmStyle: string
    cancelClass: string
    cancelStyle: string
  } = $props()
</script>

{#if armed}
  <button onclick={onconfirm} {disabled} class={confirmClass} style={confirmStyle}>{confirmLabel}</button>
  <button onclick={oncancel} {disabled} class={cancelClass} style={cancelStyle}>{cancelLabel}</button>
{:else}
  <button onclick={onarm} disabled={idleDisabled ?? disabled} class={idleClass} style={idleStyle}>{label}</button>
{/if}
