import { useState } from 'react';
import { render, screen } from '@testing-library/react';
import type { Source, Table } from '@hasura/shared/types';
import { BrowseRowsContainer } from './BrowseRowsContainer';
import { getUniqueTableKey } from './hooks/useInitialWhereAndOrderBy';

vi.mock('@hasura/metadata/data-source', () => ({
  useTableColumns: () => ({ data: undefined }),
}));

let mountCount = 0;

// stands in for the stateful BrowseRows (open tabs, active tab...)
vi.mock('../../BrowseRows', () => ({
  BrowseRows: ({ table }: { table: { name: string } }) => {
    const [mountId] = useState(() => ++mountCount);
    return (
      <div data-testid="browse-rows">
        {table.name}:{mountId}
      </div>
    );
  },
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = (name: string) => ({ schema: 'public', name }) as Table;

describe('BrowseRowsContainer', () => {
  beforeEach(() => {
    mountCount = 0;
  });

  it('resets the browse rows state when the route points to another table', () => {
    const { rerender } = render(
      <BrowseRowsContainer source={source} table={table('orders')} />,
    );
    expect(screen.getByTestId('browse-rows')).toHaveTextContent('orders:1');

    rerender(<BrowseRowsContainer source={source} table={table('users')} />);
    expect(screen.getByTestId('browse-rows')).toHaveTextContent('users:2');
  });

  it('keeps the state when the same table is parsed again from the URL', () => {
    const { rerender } = render(
      <BrowseRowsContainer source={source} table={table('orders')} />,
    );

    rerender(<BrowseRowsContainer source={source} table={table('orders')} />);
    expect(screen.getByTestId('browse-rows')).toHaveTextContent('orders:1');
  });

  it('resets the state when the same table name belongs to another source', () => {
    const { rerender } = render(
      <BrowseRowsContainer source={source} table={table('orders')} />,
    );

    rerender(
      <BrowseRowsContainer
        source={{ ...source, name: 'other' }}
        table={table('orders')}
      />,
    );
    expect(screen.getByTestId('browse-rows')).toHaveTextContent('orders:2');
  });
});

describe('getUniqueTableKey', () => {
  it('gives each object table its own saved filters key', () => {
    expect(
      getUniqueTableKey({ dataSourceName: 'default', table: table('orders') }),
    ).not.toBe(
      getUniqueTableKey({ dataSourceName: 'default', table: table('users') }),
    );
  });

  it('keeps the existing key format for array tables', () => {
    expect(
      getUniqueTableKey({
        dataSourceName: 'sqlite',
        table: ['main', 'orders'] as Table,
      }),
    ).toBe('sqlite.main-orders.query');
  });
});
