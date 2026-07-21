import { defineConfig } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'
import tailwindcss from '@tailwindcss/vite'

// Node's process at config-eval time (avoids a @types/node dependency just for this).
declare const process: { env: Record<string, string | undefined> }

// Dev-only: where `npm run dev` proxies API/media calls. Override with
// PET_API=http://localhost:<port> when the backend runs on a non-default port
// (e.g. a dev box where 8116 is taken by the deployed service).
const api = process.env.PET_API ?? 'http://localhost:8116'

export default defineConfig({
  plugins: [svelte(), tailwindcss()],
  server: {
    proxy: {
      '/api': api,
      '/proof': api,
    },
  },
})
