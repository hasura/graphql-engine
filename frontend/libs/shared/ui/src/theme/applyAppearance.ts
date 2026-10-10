import { Appearance, resolveInitialAppearance } from './appearance';

/**
 * The two classes this module owns on <html>. We only ever add/remove THESE —
 * never other classes — so we never clobber classes set by the app bootstrap,
 * bootstrap-css, or third parties.
 */
const APPEARANCE_CLASSES: readonly Appearance[] = ['light', 'dark'];

/**
 * Applies the appearance to the document root so that EVERYTHING picks it up,
 * not just the React tree under <Theme>:
 *  - the `light`/`dark` class drives the Tailwind `dark:` custom variant and the
 *    Radix color tokens on surfaces rendered outside the Radix <Theme> (portals
 *    to <body>, the react-hot-toast hub, legacy screens);
 *  - `color-scheme` makes native UI (scrollbars, form controls, the canvas
 *    background) match, avoiding white flashes around the themed content.
 */
export function applyAppearanceToDocument(appearance: Appearance): void {
  if (typeof document === 'undefined') {
    return;
  }
  const root = document.documentElement;
  APPEARANCE_CLASSES.forEach((cls) => root.classList.remove(cls));
  root.classList.add(appearance);
  root.style.colorScheme = appearance;
}

/**
 * Applies the stored appearance to the document root as soon as the app bundle
 * executes. Call this once from the app bootstrap so the appearance is set BEFORE
 * the first React render (and thus before any React-rendered content paints),
 * reducing — but not provably eliminating — flashing: index.html and its static
 * loading markup can still paint earlier (the app ships no inline theme script;
 * changing CSP to add one is out of scope). CSP-safe: this is bundle code, not an
 * inline `<script>`.
 */
export function initAppearance(): void {
  applyAppearanceToDocument(resolveInitialAppearance());
}
