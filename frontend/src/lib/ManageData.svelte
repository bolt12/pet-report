<script lang="ts">
  import { api, petPhotoUrl, type Profile, type Pet } from './api'
  import Avatar from './Avatar.svelte'
  import ConfirmButton from './ConfirmButton.svelte'
  import BackButton from './BackButton.svelte'
  import { friendlyError, isArchived } from './ui'

  let { profile, onProfile, onclose }: { profile: Profile; onProfile: (p: Profile) => void; onclose: () => void } = $props()

  let error = $state('')
  let busy = $state(false)
  // Which destructive action is armed for its second tap: `pet:<id>`, `before`, or `all`.
  let confirm = $state<string | null>(null)
  let beforeDate = $state('')
  let deletedNote = $state('')

  let archived = $derived(profile.pets.filter(isArchived))

  async function run(fn: () => Promise<void>) {
    if (busy) return
    busy = true
    error = ''
    confirm = null
    try {
      await fn()
    } catch (e) {
      error = friendlyError(e)
    } finally {
      busy = false
    }
  }

  const restore = (pet: Pet) =>
    run(async () => {
      await api.unarchivePet(pet.petId)
      onProfile(await api.settings())
    })
  const purge = (pet: Pet) =>
    run(async () => {
      await api.deletePet(pet.petId)
      onProfile(await api.settings())
    })
  const delMoments = (before?: string) =>
    run(async () => {
      const { deleted } = await api.deleteMoments(before)
      deletedNote = `Deleted ${deleted} ${deleted === 1 ? 'moment' : 'moments'}.`
    })
</script>

<div class="fade px-[18px] pt-[14px] pb-[120px] lg:mx-auto lg:max-w-[720px] lg:px-[24px] lg:pt-[24px] lg:pb-[60px]">
  <div class="mb-[18px] flex items-center gap-[12px]">
    <BackButton onclick={onclose} />
    <div class="font-head text-[22px] font-semibold" style="color:var(--text)">Manage data</div>
  </div>

  {#if error}<p class="mb-3 text-[13px]" style="color:#e26d5c">{error}</p>{/if}

  <!-- archived pets -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Archived pets</div>
  {#if archived.length === 0}
    <div class="mb-[22px] rounded-[16px] border border-dashed p-[14px] text-center text-[13px] leading-[1.5]" style="background:var(--surface);border-color:var(--line);color:var(--muted)">No archived pets. Removing a pet in setup keeps it here, so you can bring it back.</div>
  {:else}
    <div class="mb-[22px] flex flex-col gap-[9px]">
      {#each archived as p (p.petId)}
        <div class="rounded-[18px] border p-[11px]" style="background:var(--surface);border-color:var(--line)">
          <div class="flex items-center gap-[12px]">
            <Avatar name={p.petName} species={p.petSpecies} size={44} photo={petPhotoUrl(p.petId, p.petPhoto)} />
            <div class="min-w-0 flex-1">
              <div class="font-head text-[15.5px] font-semibold" style="color:var(--text)">{p.petName}</div>
              <div class="truncate text-[12px]" style="color:var(--muted)"><span class="capitalize">{p.petSpecies}</span>{p.petDescription ? ` · ${p.petDescription}` : ''}</div>
            </div>
            <button onclick={() => restore(p)} disabled={busy} class="flex-shrink-0 rounded-full px-[13px] py-[7px] text-[12px] font-extrabold disabled:opacity-50" style="border:none;background:rgba(163,192,143,0.18);color:var(--good)">Restore</button>
          </div>
          <div class="mt-[10px] border-t pt-[10px]" style="border-color:var(--line)">
            {#if confirm === `pet:${p.petId}`}
              <div class="mb-[8px] text-[12px] leading-[1.45]" style="color:#e26d5c">Permanently delete {p.petName}? Their moments and keepsakes go for good (a moment shared with another pet goes too). This can't be undone.</div>
            {/if}
            <div class="flex gap-[7px]">
              <ConfirmButton
                armed={confirm === `pet:${p.petId}`}
                onarm={() => (confirm = `pet:${p.petId}`)}
                onconfirm={() => purge(p)}
                oncancel={() => (confirm = null)}
                label="Delete permanently"
                confirmLabel="Delete for good"
                disabled={busy}
                idleDisabled={false}
                idleClass="bg-transparent text-[12px] font-bold"
                idleStyle="border:none;color:#e26d5c"
                confirmClass="flex-1 rounded-[12px] py-[9px] text-[12.5px] font-extrabold disabled:opacity-50"
                confirmStyle="border:none;background:rgba(226,109,92,0.2);color:#ffb3a6"
                cancelClass="flex-1 rounded-[12px] py-[9px] text-[12.5px] font-bold"
                cancelStyle="border:none;background:var(--surface2);color:var(--muted)"
              />
            </div>
          </div>
        </div>
      {/each}
    </div>
  {/if}

  <!-- moments -->
  <div class="mx-[2px] mb-[9px] text-[11px] font-extrabold tracking-wide uppercase" style="color:var(--faint)">Moments</div>
  {#if deletedNote}<div class="mb-[10px] text-[12.5px] font-bold" style="color:var(--good)">✓ {deletedNote}</div>{/if}
  <div class="mb-[22px] rounded-[18px] border p-[14px]" style="background:var(--surface);border-color:var(--line)">
    <div class="mb-[12px] text-[12.5px] leading-[1.45]" style="color:var(--muted)">Deleting moments also removes their images and any keepsakes of them. This can't be undone.</div>
    <div class="mb-[12px] flex items-center gap-[8px]">
      <input type="date" bind:value={beforeDate} class="min-w-0 flex-1 rounded-[12px] border px-[12px] py-[9px] text-[13px] outline-none" style="background:var(--bg);border-color:var(--line);color:var(--text)" />
      <ConfirmButton
        armed={confirm === 'before'}
        onarm={() => (confirm = 'before')}
        onconfirm={() => delMoments(new Date(beforeDate + 'T00:00:00').toISOString())}
        oncancel={() => (confirm = null)}
        label="Delete before"
        confirmLabel="Sure?"
        disabled={busy}
        idleDisabled={!beforeDate || busy}
        idleClass="flex-shrink-0 rounded-[12px] px-[12px] py-[9px] text-[12.5px] font-bold disabled:opacity-40"
        idleStyle="border:none;background:var(--surface2);color:var(--text)"
        confirmClass="flex-shrink-0 rounded-[12px] px-[12px] py-[9px] text-[12.5px] font-extrabold disabled:opacity-50"
        confirmStyle="border:none;background:rgba(226,109,92,0.18);color:#e26d5c"
        cancelClass="flex-shrink-0 rounded-[12px] px-[12px] py-[9px] text-[12.5px] font-bold"
        cancelStyle="border:none;background:var(--surface2);color:var(--muted)"
      />
    </div>
    {#if confirm === 'all'}
      <div class="mb-[8px] text-[12px] leading-[1.45]" style="color:#e26d5c">Delete every moment? Your whole timeline, all its images and keepsakes, goes for good.</div>
    {/if}
    <div class="{confirm === 'all' ? 'flex gap-[7px]' : ''}">
      <ConfirmButton
        armed={confirm === 'all'}
        onarm={() => (confirm = 'all')}
        onconfirm={() => delMoments()}
        oncancel={() => (confirm = null)}
        label="Delete all moments"
        confirmLabel="Delete all moments"
        disabled={busy}
        idleClass="w-full rounded-[12px] py-[10px] text-[12.5px] font-bold"
        idleStyle="border:1px solid rgba(226,109,92,0.4);background:transparent;color:#e26d5c"
        confirmClass="flex-1 rounded-[12px] py-[10px] text-[12.5px] font-extrabold disabled:opacity-50"
        confirmStyle="border:none;background:rgba(226,109,92,0.22);color:#ffb3a6"
        cancelClass="flex-1 rounded-[12px] py-[10px] text-[12.5px] font-bold"
        cancelStyle="border:none;background:var(--surface2);color:var(--muted)"
      />
    </div>
  </div>
</div>
