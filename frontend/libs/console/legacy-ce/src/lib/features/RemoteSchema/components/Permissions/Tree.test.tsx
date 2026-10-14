import { render, screen, waitFor } from '@testing-library/react';
import Tree from './Tree';
import type { FieldType, ExpandedItems } from './types';

// MOCKS (labelled): `Field` is stubbed to surface the `expanded` flag the Tree
// computes for each row; `getExpandedItems` is stubbed so the test controls
// which rows the list says should be expanded.
let expandedResult: ExpandedItems = {};

vi.mock('./Field', () => ({
  Field: ({ i, expanded }: { i: FieldType; expanded: boolean }) => (
    <div data-testid={`field-${i.name}`}>expanded:{String(expanded)}</div>
  ),
}));

vi.mock('./utils', () => ({
  getExpandedItems: () => expandedResult,
  addDepFields: (list: FieldType[]) => list,
}));

const makeList = (names: string[]): FieldType[] =>
  names.map((name) => ({ name, checked: undefined }) as unknown as FieldType);

describe('Tree (expandedItems seeding + merge)', () => {
  it('seeds expanded rows from the list on mount (lazy init)', () => {
    expandedResult = { 0: true };
    render(
      <Tree
        list={makeList(['TypeA', 'TypeB'])}
        filteredTypes={null}
        setState={vi.fn()}
      />,
    );

    expect(screen.getByTestId('field-TypeA')).toHaveTextContent(
      'expanded:true',
    );
    expect(screen.getByTestId('field-TypeB')).toHaveTextContent(
      'expanded:undefined',
    );
  });

  it('merges newly-expandable rows when the list changes and never collapses existing ones', async () => {
    expandedResult = { 0: true };
    const { rerender } = render(
      <Tree
        list={makeList(['TypeA', 'TypeB'])}
        filteredTypes={null}
        setState={vi.fn()}
      />,
    );
    expect(screen.getByTestId('field-TypeA')).toHaveTextContent(
      'expanded:true',
    );

    // New list reference; the list now reports row 1 as expandable. The merge
    // must ADD row 1 while keeping row 0 expanded (expand-only semantics).
    expandedResult = { 1: true };
    rerender(
      <Tree
        list={makeList(['TypeA', 'TypeB'])}
        filteredTypes={null}
        setState={vi.fn()}
      />,
    );

    await waitFor(() =>
      expect(screen.getByTestId('field-TypeB')).toHaveTextContent(
        'expanded:true',
      ),
    );
    expect(screen.getByTestId('field-TypeA')).toHaveTextContent(
      'expanded:true',
    );
  });

  it('does not loop when the list reference is stable across re-renders', async () => {
    expandedResult = { 0: true };
    const list = makeList(['TypeA', 'TypeB']);
    const { rerender } = render(
      <Tree list={list} filteredTypes={null} setState={vi.fn()} />,
    );
    // Same list reference -> the during-render merge must not run again.
    rerender(<Tree list={list} filteredTypes={null} setState={vi.fn()} />);

    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(screen.getByTestId('field-TypeA')).toHaveTextContent(
      'expanded:true',
    );
    expect(screen.getByTestId('field-TypeB')).toHaveTextContent(
      'expanded:undefined',
    );
  });
});
