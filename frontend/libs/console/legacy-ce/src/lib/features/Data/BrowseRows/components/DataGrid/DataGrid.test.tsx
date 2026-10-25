import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { Theme } from '@radix-ui/themes';
import type { TableColumn, TableRow } from '@hasura/metadata/data-source';
import { DataGrid } from './DataGrid';

type QueryState<T> = { data: T | undefined; isPending: boolean };

const columnsQuery: QueryState<{
  columns: TableColumn[];
  supportedOperators: [];
}> = { data: undefined, isPending: true };
const rowsQuery: QueryState<TableRow[]> = { data: undefined, isPending: true };

vi.mock('@hasura/metadata/data-source', () => ({
  useTableColumns: () => columnsQuery,
  useRows: () => ({ ...rowsQuery, isError: false, refetch: vi.fn() }),
  useColumnOperators: () => ({ data: undefined }),
  useEnumColumnValues: () => ({ data: undefined }),
  useDeleteRows: () => ({ mutateAsync: vi.fn(), isPending: false }),
  useExportRows: () => ({ onExportRows: vi.fn() }),
}));

vi.mock('../../../../DatabaseRelationships', () => ({
  useListAllDatabaseRelationships: () => ({ data: [], isFetching: false }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/shared/ui')>()),
  SkeletonList: () => <div data-testid="rows-skeleton" />,
  useDestructiveConfirm: () => vi.fn(),
}));

vi.mock('./parts/DataTableOptions', () => ({
  DataTableOptions: () => null,
}));

const idColumn: TableColumn = {
  name: 'id',
  dataType: 'integer',
  consoleDataType: 'integer',
  isPrimaryKey: true,
};

const renderDataGrid = () =>
  render(
    <Theme>
      <MemoryRouter>
        <DataGrid
          source={{ name: 'db', kind: 'postgres', tables: [] } as never}
          table={{ schema: 'public', name: 'album' }}
        />
      </MemoryRouter>
    </Theme>,
  );

describe('DataGrid loading state', () => {
  beforeEach(() => {
    columnsQuery.data = undefined;
    columnsQuery.isPending = true;
    rowsQuery.data = undefined;
    rowsQuery.isPending = true;
  });

  it('shows the skeleton, not an empty table, while the columns load', () => {
    renderDataGrid();

    expect(screen.getByTestId('rows-skeleton')).toBeInTheDocument();
    expect(screen.queryByText('No rows Available')).not.toBeInTheDocument();
  });

  it('shows the skeleton while the rows wait for their first result', () => {
    columnsQuery.data = { columns: [idColumn], supportedOperators: [] };
    columnsQuery.isPending = false;
    renderDataGrid();

    expect(screen.getByTestId('rows-skeleton')).toBeInTheDocument();
    expect(screen.queryByText('No rows Available')).not.toBeInTheDocument();
  });

  it('shows the empty table once the rows are loaded', () => {
    columnsQuery.data = { columns: [idColumn], supportedOperators: [] };
    columnsQuery.isPending = false;
    rowsQuery.data = [];
    rowsQuery.isPending = false;
    renderDataGrid();

    expect(screen.getByText('No rows Available')).toBeInTheDocument();
  });

  it('shows the empty table for a table without columns', () => {
    // the rows query stays disabled, so it never leaves the pending state
    columnsQuery.data = { columns: [], supportedOperators: [] };
    columnsQuery.isPending = false;
    renderDataGrid();

    expect(screen.queryByTestId('rows-skeleton')).not.toBeInTheDocument();
    expect(screen.getByText('No rows Available')).toBeInTheDocument();
  });

  it('renders the rows once loaded', () => {
    columnsQuery.data = { columns: [idColumn], supportedOperators: [] };
    columnsQuery.isPending = false;
    rowsQuery.data = [{ id: 7 }];
    rowsQuery.isPending = false;
    renderDataGrid();

    expect(screen.getByText('7')).toBeInTheDocument();
  });
});
