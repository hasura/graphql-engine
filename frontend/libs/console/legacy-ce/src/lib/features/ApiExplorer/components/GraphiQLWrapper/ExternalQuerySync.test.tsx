/**
 * Integration coverage for the REAL ExternalQuerySync component (not just the
 * pure planExternalQuerySync helper): it must read the editor from the GraphiQL
 * store via `useGraphiQL` and call `queryEditor.setValue` only for genuinely new
 * external queries — never clobbering the user's own edits, and tolerating the
 * editor not being ready yet.
 *
 * jsdom can't instantiate Monaco, so the store boundary (`useGraphiQL`) is mocked
 * with a stable fake editor; everything above it (the component's effect/ref
 * wiring and the real planExternalQuerySync) runs for real.
 */
import { render } from '@testing-library/react';
import { vi } from 'vitest';

const h = vi.hoisted(() => {
  const state = {
    editorValue: '',
    hasEditor: true,
    setValueCalls: [] as string[],
  };
  // Stable reference while present, so the component's effect deps behave like
  // production (the real store hands back the same editor instance each render).
  const editor = {
    getValue: () => state.editorValue,
    setValue: (v: string) => {
      state.setValueCalls.push(v);
      state.editorValue = v;
    },
  };
  return { state, editor };
});

vi.mock('@graphiql/react', () => ({
  useGraphiQL: (selector: (s: { queryEditor: unknown }) => unknown) =>
    selector({ queryEditor: h.state.hasEditor ? h.editor : null }),
}));

import { ExternalQuerySync } from './ExternalQuerySync';

beforeEach(() => {
  h.state.editorValue = '';
  h.state.hasEditor = true;
  h.state.setValueCalls = [];
});

describe('ExternalQuerySync', () => {
  it('does NOT overwrite the editor on mount (initialQuery already seeded it)', () => {
    h.state.editorValue = 'query { me }';
    render(<ExternalQuerySync externalQuery="query { me }" />);
    expect(h.state.setValueCalls).toEqual([]);
  });

  it('applies a genuinely new external query (e.g. a late query_file load)', () => {
    h.state.editorValue = 'query { a }';
    const { rerender } = render(
      <ExternalQuerySync externalQuery="query { a }" />,
    );
    expect(h.state.setValueCalls).toEqual([]);

    rerender(<ExternalQuerySync externalQuery="query { b }" />);
    expect(h.state.setValueCalls).toEqual(['query { b }']);
    expect(h.state.editorValue).toBe('query { b }');
  });

  it("does NOT clobber the user's own edit echoing back through the parent", () => {
    h.state.editorValue = 'query { a }';
    const { rerender } = render(
      <ExternalQuerySync externalQuery="query { a }" />,
    );

    // user types -> editor changes -> onEditQuery -> parent setQuery -> prop
    h.state.editorValue = 'query { typed }';
    rerender(<ExternalQuerySync externalQuery="query { typed }" />);
    expect(h.state.setValueCalls).toEqual([]);

    // and a repeat of the same external value stays a no-op
    rerender(<ExternalQuerySync externalQuery="query { typed }" />);
    expect(h.state.setValueCalls).toEqual([]);
  });

  it('is a no-op when the editor is not ready, then syncs once it appears', () => {
    h.state.hasEditor = false;
    const { rerender } = render(
      <ExternalQuerySync externalQuery="query { a }" />,
    );
    expect(h.state.setValueCalls).toEqual([]); // no crash, nothing applied

    // editor mounts and a new external query arrives
    h.state.hasEditor = true;
    h.state.editorValue = 'stale';
    rerender(<ExternalQuerySync externalQuery="query { b }" />);
    expect(h.state.setValueCalls).toEqual(['query { b }']);
  });
});
