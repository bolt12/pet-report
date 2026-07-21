// A tiny cross-component theme store (Svelte 5 runes), persisted to localStorage.

export type ThemeName = 'night' | 'day'

const THEME_KEY = 'pr-theme'
const MOTION_KEY = 'pr-reduce-motion'

function initTheme(): ThemeName {
  const saved = localStorage.getItem(THEME_KEY)
  return saved === 'day' ? 'day' : 'night'
}

export const theme = $state({
  name: initTheme(),
  reduceMotion: localStorage.getItem(MOTION_KEY) === '1',
})

export function toggleTheme() {
  theme.name = theme.name === 'night' ? 'day' : 'night'
  localStorage.setItem(THEME_KEY, theme.name)
}

export function toggleMotion() {
  theme.reduceMotion = !theme.reduceMotion
  localStorage.setItem(MOTION_KEY, theme.reduceMotion ? '1' : '0')
}
