/**
 * Unit coverage for the GraphiQL <-> app-appearance bridge. We assert the
 * contract at the boundary: GraphiQLThemeSync calls GraphiQL's `setTheme` store
 * action with the current appearance, and again whenever the appearance changes.
 * The GraphiQL store (`useGraphiQLActions`) and the app appearance
 * (`useOptionalAppearance`) are mocked so this stays a focused unit — the live
 * Monaco recolor + query preservation are covered by the browser harness.
 */
import { describe, expect, it, vi } from 'vitest';
import { render } from '@testing-library/react';

const setTheme = vi.fn();
vi.mock('@graphiql/react', () => ({
  useGraphiQLActions: () => ({ setTheme }),
}));

const appearanceState = { appearance: 'light' as 'light' | 'dark' };
vi.mock('@hasura/shared/ui', () => ({
  useOptionalAppearance: () => appearanceState,
}));

import { GraphiQLThemeSync } from './GraphiQLThemeSync';

describe('GraphiQLThemeSync', () => {
  it('pushes the current appearance into GraphiQL and updates on change', () => {
    appearanceState.appearance = 'light';
    setTheme.mockClear();
    const { container, rerender } = render(<GraphiQLThemeSync />);
    // renders nothing, drives the store instead
    expect(container).toBeEmptyDOMElement();
    expect(setTheme).toHaveBeenLastCalledWith('light');

    appearanceState.appearance = 'dark';
    rerender(<GraphiQLThemeSync />);
    expect(setTheme).toHaveBeenLastCalledWith('dark');
  });
});
