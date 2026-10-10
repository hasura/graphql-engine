import { useEffect } from 'react';
import { useGraphiQLActions } from '@graphiql/react';
import { useOptionalAppearance } from '@hasura/shared/ui';

/**
 * Keeps GraphiQL's own theme (its chrome + the Monaco editors) in sync with the
 * console appearance. `defaultTheme` only seeds GraphiQL's first render, so this
 * calls the official `setTheme` store action on every appearance change. That
 * mutates the GraphiQL store in place (and internally runs
 * `monaco.editor.setTheme`), so the editors recolor WITHOUT remounting — query,
 * variables, tabs and history are preserved across a light/dark toggle.
 *
 * Must be rendered inside `<GraphiQL>` so the store provider exists (same pattern
 * as ExternalQuerySync).
 */
export function GraphiQLThemeSync() {
  const { setTheme } = useGraphiQLActions();
  const appearance = useOptionalAppearance()?.appearance ?? 'light';

  useEffect(() => {
    setTheme(appearance);
  }, [appearance, setTheme]);

  return null;
}
