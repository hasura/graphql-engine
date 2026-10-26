import React from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { DataGrid } from './DataGrid';

const queries = vi.hoisted(() => ({
  relationships: {
    data: [],
    isLoading: false,
    isFetching: false,
  },
}));

vi.mock('@hasura/metadata/data-source', () => ({
  useTableColumns: () => ({
    data: {
      columns: [{ name: 'id', isPrimaryKey: true }],
      supportedOperators: [],
    },
    isPending: false,
  }),
  useRows: () => ({
    data: [{ id: 1 }],
    isPending: false,
    isFetching: false,
    isError: false,
    refetch: vi.fn(),
  }),
  useColumnOperators: () => ({}),
  useEnumColumnValues: () => ({}),
  useDeleteRows: () => ({ mutateAsync: vi.fn(), isPending: false }),
  useExportRows: () => ({ onExportRows: vi.fn() }),
}));

vi.mock('../../../../DatabaseRelationships', () => ({
  useListAllDatabaseRelationships: () => queries.relationships,
}));

vi.mock('@hasura/shared/ui', () => ({
  SkeletonList: () => <div data-testid="rows-skeleton" />,
  ErrorCard: () => null,
  useDestructiveConfirm: () => vi.fn(),
}));

vi.mock('./parts/DataTableOptions', () => ({
  DataTableOptions: () => null,
}));
vi.mock('./QueryDialog', () => ({ QueryDialog: () => null }));
vi.mock('./DataGrid.utils', () => ({
  adaptSelectedRowIdsToWhereClause: () => [],
}));

// The real wrapper owns the open-row/edit-row state. This stateful stand-in
// isolates whether DataGrid preserves that child across background refetches.
vi.mock('./parts/ReactTableWrapper', () => ({
  ReactTableWrapper: () => {
    const [draft, setDraft] = React.useState('saved original');
    return (
      <input
        aria-label="Unsaved row draft"
        value={draft}
        onChange={(event) => setDraft(event.target.value)}
      />
    );
  },
}));

const grid = (
  <DataGrid
    source={{ name: 'db', kind: 'postgres', tables: [] } as never}
    table={{ schema: 'public', name: 'album' }}
  />
);

describe('DataGrid relationship loading', () => {
  beforeEach(() => {
    queries.relationships.isLoading = false;
    queries.relationships.isFetching = false;
  });

  it('preserves child edit state during and after a background refetch', () => {
    const { rerender } = render(grid);
    fireEvent.change(screen.getByRole('textbox'), {
      target: { value: 'UNSAVED local draft' },
    });

    queries.relationships.isFetching = true;
    rerender(React.cloneElement(grid));

    expect(screen.queryByTestId('rows-skeleton')).not.toBeInTheDocument();
    expect(screen.getByRole('textbox')).toHaveValue('UNSAVED local draft');

    queries.relationships.isFetching = false;
    rerender(React.cloneElement(grid));

    expect(screen.getByRole('textbox')).toHaveValue('UNSAVED local draft');
  });

  it('still shows the skeleton during the first relationship fetch', () => {
    queries.relationships.isLoading = true;
    queries.relationships.isFetching = true;
    const { rerender } = render(grid);

    expect(screen.getByTestId('rows-skeleton')).toBeInTheDocument();
    expect(screen.queryByRole('textbox')).not.toBeInTheDocument();

    queries.relationships.isLoading = false;
    queries.relationships.isFetching = false;
    rerender(React.cloneElement(grid));

    expect(screen.queryByTestId('rows-skeleton')).not.toBeInTheDocument();
    expect(screen.getByRole('textbox')).toHaveValue('saved original');
  });
});
