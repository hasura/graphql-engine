import { useContext } from 'react';
import { render, screen } from '@testing-library/react';
import { TableProvider, tableContext } from './TableProvider';
import { getTablesKey, rootTableContext } from './RootTableProvider';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { Tables } from './types';

// Regression for the set-state-in-effect -> "adjust state while rendering" fix:
// derived columns/relationships must sync from the tables metadata without an
// effect, update when the metadata changes, call loadRelationships as a side
// effect, and not re-render in a loop.

const makeTables = (albumColumns: string[]): Tables =>
  [
    {
      table: ['Album'],
      columns: albumColumns.map((name) => ({
        name,
        dataType: 'int',
        graphQLProperties: null,
      })),
      relationships: [],
      dataSource: { name: 'pg', kind: 'postgres' },
      computedFields: [],
    },
  ] as unknown as Tables;

const renderSpy = vi.fn();

const Consumer = () => {
  const { columns } = useContext(tableContext);
  renderSpy();
  return (
    <div data-testid="columns">{columns.map((c) => c.name).join(',')}</div>
  );
};

const wrapper = (tables: Tables, loadRelationships = vi.fn()) => {
  const rootTable = tables.find((t) => JSON.stringify(t.table) === '["Album"]');
  return (
    <rootTableContext.Provider
      value={{
        table: ['Album'],
        tables,
        tablesKey: getTablesKey(tables),
        rootTable,
      }}
    >
      <rowPermissionsContext.Provider
        value={
          {
            comparators: {},
            operators: {},
            permissions: {},
            setValue: () => undefined,
            setKey: () => undefined,
            setPermissions: () => undefined,
            loadRelationships,
          } as any
        }
      >
        <TableProvider table={['Album']}>
          <Consumer />
        </TableProvider>
      </rowPermissionsContext.Provider>
    </rootTableContext.Provider>
  );
};

describe('TableProvider', () => {
  beforeEach(() => {
    renderSpy.mockClear();
  });

  it('derives columns from the matching table on the initial render', () => {
    render(wrapper(makeTables(['AlbumId', 'Title'])));
    expect(screen.getByTestId('columns')).toHaveTextContent('AlbumId,Title');
  });

  it('updates derived columns when the tables metadata changes', () => {
    const loadRelationships = vi.fn();
    const { rerender } = render(
      wrapper(makeTables(['AlbumId']), loadRelationships),
    );
    expect(screen.getByTestId('columns')).toHaveTextContent('AlbumId');

    rerender(wrapper(makeTables(['AlbumId', 'Name']), loadRelationships));
    expect(screen.getByTestId('columns')).toHaveTextContent('AlbumId,Name');
  });

  it('calls loadRelationships for the matching table', () => {
    const loadRelationships = vi.fn();
    render(wrapper(makeTables(['AlbumId']), loadRelationships));
    expect(loadRelationships).toHaveBeenCalledWith([]);
  });

  it('does not re-render in a loop for stable inputs', () => {
    render(wrapper(makeTables(['AlbumId'])));
    // initial render + the single allowed during-render sync pass
    expect(renderSpy.mock.calls.length).toBeLessThanOrEqual(3);
  });

  it('keeps the relationships a permission can use', () => {
    const remoteDatabaseRelationship = (
      name: string,
      toSource: string,
      mapping: Record<string, string>,
    ) => ({
      name,
      fromSource: 'pg',
      fromTable: ['Album'],
      relationshipType: 'Object',
      type: 'remoteDatabaseRelationship',
      definition: { toSource, toTable: ['Artist'], mapping },
    });

    const tables = [
      {
        ...makeTables(['AlbumId'])[0],
        relationships: [
          {
            name: 'local',
            fromSource: 'pg',
            fromTable: ['Album'],
            relationshipType: 'Object',
            type: 'localRelationship',
            definition: { toTable: ['Artist'], mapping: { id: 'id' } },
          },
          remoteDatabaseRelationship('to_postgres', 'pg2', { id: 'id' }),
          remoteDatabaseRelationship('to_mssql', 'mssql', { id: 'id' }),
          remoteDatabaseRelationship('multi_column', 'pg2', {
            id: 'id',
            code: 'code',
          }),
          {
            name: 'remote_schema',
            fromSource: 'pg',
            fromTable: ['Album'],
            relationshipType: 'Remote',
            type: 'remoteSchemaRelationship',
            definition: {},
          },
        ],
      },
      {
        table: ['Artist'],
        columns: [],
        relationships: [],
        dataSource: { name: 'mssql', kind: 'mssql' },
        computedFields: [],
      },
    ] as unknown as Tables;

    const Relationships = () => {
      const { relationships } = useContext(tableContext);
      return (
        <div data-testid="relationships">
          {relationships.map((r) => r.name).join(',')}
        </div>
      );
    };

    render(
      <rootTableContext.Provider
        value={{
          table: ['Album'],
          tables,
          tablesKey: getTablesKey(tables),
          rootTable: tables[0],
        }}
      >
        <rowPermissionsContext.Provider
          value={{ loadRelationships: vi.fn() } as any}
        >
          <TableProvider table={['Album']}>
            <Relationships />
          </TableProvider>
        </rowPermissionsContext.Provider>
      </rootTableContext.Provider>,
    );

    // the server only accepts a remote database relationship on a single
    // column, to a source of any kind
    expect(screen.getByTestId('relationships')).toHaveTextContent(
      /^local,to_postgres,to_mssql$/,
    );
  });
});
