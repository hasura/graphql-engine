import { render, screen, fireEvent } from '@testing-library/react';
import { vi } from 'vitest';
import type { Source, Table } from '@hasura/shared/types';
import { BrowseRows } from './BrowseRows';

vi.mock('./BrowseRows.utils', () => ({
  setWhereAndSortToUrl: vi.fn(),
}));

// Lightweight DataGrid: shows the options it received and lets the test open a
// relationship tab by invoking the onRelationshipOpen callback.
vi.mock('./components/DataGrid/DataGrid', () => ({
  DataGrid: ({
    options,
    onRelationshipOpen,
  }: {
    options?: { tag?: string };
    onRelationshipOpen?: (p: unknown) => void;
  }) => (
    <div>
      <div data-testid="grid-options">{options?.tag ?? 'none'}</div>
      <button
        data-testid="open-rel"
        onClick={() =>
          onRelationshipOpen?.({
            table: { schema: 'public', name: 'child' },
            relationshipName: 'rel1',
            parentOptions: options,
            parentTableFilter: [],
          })
        }
      >
        open relationship
      </button>
    </div>
  ),
}));

vi.mock('./components/DataGrid/parts/TableTabView', () => ({
  TableTabView: ({
    items,
    activeTab,
  }: {
    items: { value: string; label: string; content: React.ReactNode }[];
    activeTab: string;
  }) => (
    <div>
      <div data-testid="tab-count">{items.length}</div>
      {items.map((it) => (
        <div key={it.value}>{it.value === activeTab ? it.content : null}</div>
      ))}
    </div>
  ),
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { schema: 'public', name: 'orders' } as unknown as Table;

describe('BrowseRows options-change reset (adjust-state-during-render)', () => {
  it('renders a single tab on initial render', () => {
    render(
      <BrowseRows
        source={source}
        table={table}
        options={{ tag: 'v1' } as never}
        onUpdateOptions={vi.fn()}
      />,
    );
    expect(screen.getByTestId('tab-count')).toHaveTextContent('1');
    expect(screen.getByTestId('grid-options')).toHaveTextContent('v1');
  });

  it('resets back to a single tab when the options prop changes', () => {
    const { rerender } = render(
      <BrowseRows
        source={source}
        table={table}
        options={{ tag: 'v1' } as never}
        onUpdateOptions={vi.fn()}
      />,
    );

    // open a relationship -> two tabs
    fireEvent.click(screen.getByTestId('open-rel'));
    expect(screen.getByTestId('tab-count')).toHaveTextContent('2');

    // a new options prop resets to the single original tab
    rerender(
      <BrowseRows
        source={source}
        table={table}
        options={{ tag: 'v2' } as never}
        onUpdateOptions={vi.fn()}
      />,
    );
    expect(screen.getByTestId('tab-count')).toHaveTextContent('1');
  });

  it('does not reset (or loop) when re-rendered with the same options reference', () => {
    const options = { tag: 'v1' } as never;
    const { rerender } = render(
      <BrowseRows
        source={source}
        table={table}
        options={options}
        onUpdateOptions={vi.fn()}
      />,
    );

    fireEvent.click(screen.getByTestId('open-rel'));
    expect(screen.getByTestId('tab-count')).toHaveTextContent('2');

    // same options reference -> the guard prevents a reset and any render loop
    expect(() =>
      rerender(
        <BrowseRows
          source={source}
          table={table}
          options={options}
          onUpdateOptions={vi.fn()}
        />,
      ),
    ).not.toThrow();
    expect(screen.getByTestId('tab-count')).toHaveTextContent('2');
  });
});
