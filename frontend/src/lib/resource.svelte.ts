// The shared load-once/reload pattern several screens hand-rolled: set loading,
// clear the error, await the loader, turn a thrown error into a calm line, and
// latch `loaded` after the first settle. A monotonically-increasing request id,
// compared after the await, drops a stale in-flight response so a slow reload can
// never overwrite a newer one (the alive/generation guard the screens did by hand).

import { friendlyError } from './ui'

export interface Resource<T> {
  readonly data: T | null
  readonly error: string
  // True while a fetch is in flight (goes true on every reload).
  readonly loading: boolean
  // True once the first attempt has settled (success OR error), matching the
  // screens' `finally { loaded = true }`. Never returns to false.
  readonly loaded: boolean
  reload: () => Promise<void>
}

export function createResource<T>(loader: () => Promise<T>): Resource<T> {
  let data = $state<T | null>(null)
  let error = $state('')
  let loading = $state(false)
  let loaded = $state(false)
  // Bumped per request; only the newest request is allowed to write state back.
  let gen = 0

  async function reload(): Promise<void> {
    const mine = ++gen
    loading = true
    error = ''
    try {
      const result = await loader()
      if (mine !== gen) return // a newer reload superseded us; drop this result
      data = result
    } catch (e) {
      if (mine !== gen) return
      error = friendlyError(e)
    } finally {
      if (mine === gen) {
        loading = false
        loaded = true
      }
    }
  }

  return {
    get data() {
      return data
    },
    get error() {
      return error
    },
    get loading() {
      return loading
    },
    get loaded() {
      return loaded
    },
    reload,
  }
}
