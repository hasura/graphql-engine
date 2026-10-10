/**
 * Dark-mode "appearance" primitives: the type, the persistent-storage key, and
 * SSR-/blocked-storage-safe read/write helpers. Appearance is a per-browser-
 * profile preference (localStorage), NOT server metadata.
 *
 * The default is LIGHT, which preserves the console's existing look for everyone
 * who has never toggled. Only an explicit user toggle switches to dark — there is
 * no automatic `prefers-color-scheme` following (binary, explicit by request).
 */
export type Appearance = 'light' | 'dark';

export const APPEARANCE_STORAGE_KEY = 'hasura-console:appearance';

export const DEFAULT_APPEARANCE: Appearance = 'light';

export const isAppearance = (value: unknown): value is Appearance =>
  value === 'light' || value === 'dark';

/**
 * Reads the stored appearance. Returns null when there is nothing valid stored,
 * when `localStorage` is unavailable (SSR, sandboxed iframe) or access throws
 * (Safari private mode, storage disabled) — callers fall back to the default.
 */
export function readStoredAppearance(): Appearance | null {
  try {
    if (typeof window === 'undefined' || !window.localStorage) {
      return null;
    }
    const stored = window.localStorage.getItem(APPEARANCE_STORAGE_KEY);
    return isAppearance(stored) ? stored : null;
  } catch {
    return null;
  }
}

/** Persists the appearance; silently no-ops when storage is unavailable/blocked. */
export function writeStoredAppearance(appearance: Appearance): void {
  try {
    if (typeof window === 'undefined' || !window.localStorage) {
      return;
    }
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, appearance);
  } catch {
    /* storage blocked (private mode / disabled / quota) — preference stays
       in-memory for this session only. */
  }
}

export function resolveInitialAppearance(): Appearance {
  return readStoredAppearance() ?? DEFAULT_APPEARANCE;
}
