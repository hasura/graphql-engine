import {
  createContext,
  ReactNode,
  useCallback,
  useContext,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
} from 'react';
import {
  Appearance,
  resolveInitialAppearance,
  writeStoredAppearance,
} from './appearance';
import { applyAppearanceToDocument } from './applyAppearance';

export type ThemeContextValue = {
  appearance: Appearance;
  setAppearance: (appearance: Appearance) => void;
  toggleAppearance: () => void;
};

const ThemeContext = createContext<ThemeContextValue | undefined>(undefined);

// Runs before paint on the client; falls back to useEffect where there is no DOM
// (tests without jsdom / any SSR) to avoid React's layout-effect warning.
const useIsomorphicLayoutEffect =
  typeof window !== 'undefined' ? useLayoutEffect : useEffect;

/** Hook for consumers that MUST be inside the provider (throws otherwise). */
export function useAppearance(): ThemeContextValue {
  const ctx = useContext(ThemeContext);
  if (ctx === undefined) {
    throw new Error(
      'useAppearance must be used within an AppTheme/ThemeProvider',
    );
  }
  return ctx;
}

/** Non-throwing variant for components that may render outside the provider. */
export function useOptionalAppearance(): ThemeContextValue | undefined {
  return useContext(ThemeContext);
}

/**
 * Owns the app-wide appearance (light/dark) and keeps the document root in sync.
 *
 * - initial value is read synchronously from storage (default LIGHT), so the
 *   first React render is already correct;
 * - the document root class + color-scheme are applied in a layout effect and
 *   whenever the appearance changes;
 * - changes are persisted to the browser profile (localStorage) AFTER commit
 *   (never inside a render/updater — StrictMode may run those twice or discard
 *   them), so they survive reload;
 * - on the owning provider's unmount it restores the document root's prior
 *   appearance class + color-scheme (leaving unrelated classes alone), so an
 *   independently mounted/unmounted provider (tests, Storybook) leaves no trace.
 *
 * Should be mounted ONCE at the app root (inside AppTheme). Call `initAppearance()`
 * from the app bootstrap so the stored appearance is applied before the first
 * React render (CSP-safe: bundle code, not an inline <script>).
 */
export function ThemeProvider({ children }: { children?: ReactNode }) {
  // If a ThemeProvider already exists above us (e.g. Storybook wraps every story
  // in AppTheme and a story wraps itself in AppTheme again), do NOT create a
  // second writer to document/localStorage — reuse the parent so there is a
  // single source of truth.
  const parent = useContext(ThemeContext);

  const [appearance, setAppearanceState] = useState<Appearance>(
    resolveInitialAppearance,
  );

  // Snapshot the root's prior appearance class + color-scheme on mount and
  // restore them on unmount. Defined BEFORE the apply effect so it captures the
  // pre-apply state, and so its cleanup (restore) runs AFTER the apply effect's
  // on unmount. Only the appearance class + color-scheme are touched.
  useIsomorphicLayoutEffect(() => {
    if (parent || typeof document === 'undefined') {
      return undefined;
    }
    const el = document.documentElement;
    const priorClass: Appearance | null = el.classList.contains('dark')
      ? 'dark'
      : el.classList.contains('light')
        ? 'light'
        : null;
    const priorColorScheme = el.style.colorScheme;
    return () => {
      el.classList.remove('light', 'dark');
      if (priorClass) {
        el.classList.add(priorClass);
      }
      el.style.colorScheme = priorColorScheme;
    };
  }, [parent]);

  useIsomorphicLayoutEffect(() => {
    if (parent) {
      return;
    }
    applyAppearanceToDocument(appearance);
  }, [appearance, parent]);

  // Persist after commit. `lastPersisted` guards the initial mount (don't rewrite
  // the value we just read) and StrictMode remounts (same value -> no write).
  const lastPersisted = useRef(appearance);
  useEffect(() => {
    if (parent || appearance === lastPersisted.current) {
      return;
    }
    lastPersisted.current = appearance;
    writeStoredAppearance(appearance);
  }, [appearance, parent]);

  const setAppearance = useCallback((next: Appearance) => {
    setAppearanceState(next);
  }, []);

  const toggleAppearance = useCallback(() => {
    setAppearanceState((prev) => (prev === 'dark' ? 'light' : 'dark'));
  }, []);

  const value = useMemo<ThemeContextValue>(
    () => ({ appearance, setAppearance, toggleAppearance }),
    [appearance, setAppearance, toggleAppearance],
  );

  // Nested provider: pass the parent's context through unchanged.
  if (parent) {
    return <>{children}</>;
  }

  return (
    <ThemeContext.Provider value={value}>{children}</ThemeContext.Provider>
  );
}
