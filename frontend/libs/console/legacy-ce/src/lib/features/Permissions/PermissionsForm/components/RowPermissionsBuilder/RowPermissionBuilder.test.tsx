import {
  act,
  fireEvent,
  renderHook,
  screen,
  waitFor,
} from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { FormProvider, useForm } from 'react-hook-form';
import { testRenderWithClient, testWrapper } from '@hasura/shared/testing';
import type { TableColumn } from '@hasura/metadata/data-source';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { RowPermissionBuilder } from './RowPermissionBuilder';
import { usePermissionTables } from './hooks/usePermissionTables';

// jsdom lacks ResizeObserver, which the _exists table picker (Radix scroll-area)
// depends on.
beforeAll(() => {
  globalThis.ResizeObserver =
    globalThis.ResizeObserver ??
    class {
      observe() {
        /* noop */
      }
      unobserve() {
        /* noop */
      }
      disconnect() {
        /* noop */
      }
    };
});

const table = (name: string) => ({ schema: 'public', name });

const column = (name: string): TableColumn => ({
  name,
  dataType: 'text',
  consoleDataType: 'string',
});

const columns: Record<string, TableColumn[]> = {
  orders: [column('id'), column('contract_id')],
  contracts: [column('id'), column('owner_id')],
  links: [column('contract_id'), column('ddn_id')],
  ddns: [column('id'), column('owner_id')],
  users: [column('id')],
};

// the foreign keys of the tables with object relationships on their own keys
const foreignKeys: Record<string, unknown[]> = {
  orders: [
    {
      from: { table: table('orders'), columns: ['contract_id'] },
      to: { table: table('contracts'), columns: ['id'] },
    },
  ],
  ddns: [
    {
      from: { table: table('ddns'), columns: ['owner_id'] },
      to: { table: table('users'), columns: ['id'] },
    },
  ],
};

const getFKRelationships = vi.fn(
  async ({ table: t }: { table: { name: string } }) =>
    foreignKeys[t.name] ?? [],
);

const getTableColumns = vi.fn(
  async ({ table: t }: { table: { name: string } }) => columns[t.name] ?? [],
);

vi.mock('@hasura/metadata/data-source', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/metadata/data-source')>()),
  getDatabaseMethods: () => ({
    introspection: {
      getTableColumns,
      getSupportedOperators: () => [],
      getFKRelationships,
    },
  }),
}));

// the functions of the computed fields that return a table
const runSQL = vi.fn(async () => ({
  result_type: 'TuplesOk',
  result: [
    ['function_schema', 'function_name'],
    ['public', 'user_orders'],
  ],
}));

vi.mock('@hasura/metadata/api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/metadata/api')>()),
  runSQL: (...args: unknown[]) => runSQL(...(args as [])),
}));

vi.mock('./hooks/usePermissionComparators', () => ({
  usePermissionComparators: () => ({}),
}));

const source = { name: 'default', kind: 'postgres' };

const metadata = {
  version: 3,
  sources: [
    {
      ...source,
      tables: [
        {
          table: table('orders'),
          // on the table's own foreign key
          object_relationships: [
            {
              name: 'contract',
              using: { foreign_key_constraint_on: 'contract_id' },
            },
          ],
        },
        {
          table: table('contracts'),
          // on the foreign key of the remote table
          array_relationships: [
            {
              name: 'links',
              using: {
                foreign_key_constraint_on: {
                  table: table('links'),
                  column: 'contract_id',
                },
              },
            },
          ],
        },
        {
          table: table('links'),
          object_relationships: [
            {
              name: 'ddn',
              using: {
                manual_configuration: {
                  remote_table: table('ddns'),
                  column_mapping: { ddn_id: 'id' },
                },
              },
            },
          ],
        },
        {
          table: table('ddns'),
          object_relationships: [
            { name: 'owner', using: { foreign_key_constraint_on: 'owner_id' } },
          ],
        },
        { table: table('users') },
      ],
    },
  ],
} as never;

const permissions = {
  _or: [
    { contract: { owner_id: { _eq: 'X-Hasura-User-Id' } } },
    {
      contract: {
        links: { ddn: { owner: { id: { _eq: 'X-Hasura-User-Id' } } } },
      },
    },
  ],
};

const formRenderSpy = vi.fn();

const FormWithFilter = ({ filter }: { filter: Record<string, unknown> }) => {
  const methods = useForm({ defaultValues: { filter } });
  formRenderSpy();
  return (
    <Theme>
      <FormProvider {...methods}>
        <RowPermissionBuilder
          permissionsKey="filter"
          table={table('orders')}
          source={source as never}
          metadata={metadata}
        />
      </FormProvider>
    </Theme>
  );
};

const Form = () => <FormWithFilter filter={permissions} />;

const selectedKeys = () =>
  Array.from(document.querySelectorAll('[class*="singleValue"]')).map(
    (element) => element.textContent,
  );

describe('RowPermissionBuilder', () => {
  it('resolves the nested relationships from the metadata and the foreign keys', async () => {
    testRenderWithClient(<Form />);

    await waitFor(() =>
      expect(selectedKeys()).toEqual([
        '_or',
        'contract',
        'owner_id',
        '_eq',
        'contract',
        'links',
        'ddn',
        'owner',
        'id',
        '_eq',
      ]),
    );

    // only the tables with relationships on their own foreign keys need them
    const tablesWithForeignKeysLoaded = getFKRelationships.mock.calls.map(
      ([props]) => props.table.name,
    );
    expect(new Set(tablesWithForeignKeysLoaded)).toEqual(
      new Set(['orders', 'ddns']),
    );
  });

  it("doesn't re-render the whole form on an edit", async () => {
    testRenderWithClient(<Form />);
    // once the relationships are loaded, as the entries remount meanwhile
    await waitFor(() => expect(selectedKeys()).toHaveLength(10));

    formRenderSpy.mockClear();
    fireEvent.change(
      screen.getByTestId('_or.0.contract.owner_id._eq-value-input'),
      { target: { value: 'X-Hasura-Org-Id' } },
    );

    await waitFor(() =>
      expect(screen.getByTestId('row-permission-builder')).toHaveAttribute(
        'data-state',
        expect.stringContaining('X-Hasura-Org-Id'),
      ),
    );
    // the builder watches its value on its own
    expect(formRenderSpy).not.toHaveBeenCalled();
  });

  // Regression test for issue #10907: an `_exists` references its table through
  // `_table`, not a relationship, so its columns were never introspected and the
  // `_where` operators rendered blank. The builder must queue that table.
  it('loads the table of an existing _exists so its _where columns render', async () => {
    getTableColumns.mockClear();
    testRenderWithClient(
      <FormWithFilter
        filter={{
          _exists: {
            _table: table('users'),
            _where: { id: { _eq: 'X-Hasura-User-Id' } },
          },
        }}
      />,
    );

    // the column under `_where` resolves only once `users` has been introspected
    await waitFor(() => expect(selectedKeys()).toContain('id'));
    expect(selectedKeys()).toEqual(['_exists', 'id', '_eq']);

    const introspectedTables = getTableColumns.mock.calls.map(
      ([props]) => props.table.name,
    );
    expect(introspectedTables).toContain('users');
  });
});

describe('usePermissionTables', () => {
  it('keeps the loaded tables while the ones of a relationship load', async () => {
    const { result } = renderHook(
      () =>
        usePermissionTables({
          source: source as never,
          table: table('orders'),
          metadata,
        }),
      { wrapper: testWrapper },
    );
    await waitFor(() => expect(result.current.isLoading).toBe(false));

    act(() =>
      result.current.setTablesToLoad((tablesToLoad) => [
        ...tablesToLoad,
        { table: table('contracts'), source: 'default' },
      ]),
    );

    // the builder isn't emptied nor disabled meanwhile
    expect(result.current.isLoading).toBe(false);
    expect(result.current.tables).not.toHaveLength(0);

    await waitFor(() =>
      expect(
        result.current.tables.find((t) =>
          areTablesEqual(t.table, table('contracts')),
        )?.columns,
      ).toHaveLength(2),
    );
  });

  it("doesn't introspect the loaded tables again", async () => {
    getTableColumns.mockClear();
    const { result } = renderHook(
      () =>
        usePermissionTables({
          source: source as never,
          table: table('orders'),
          metadata,
        }),
      { wrapper: testWrapper },
    );
    await waitFor(() => expect(result.current.isLoading).toBe(false));

    act(() =>
      result.current.setTablesToLoad((tablesToLoad) => [
        ...tablesToLoad,
        { table: table('contracts'), source: 'default' },
      ]),
    );
    await waitFor(() =>
      expect(
        result.current.tables.find((t) =>
          areTablesEqual(t.table, table('contracts')),
        )?.columns,
      ).toHaveLength(2),
    );

    const introspectedTables = getTableColumns.mock.calls.map(
      ([props]) => props.table.name,
    );
    expect(introspectedTables).toEqual(['orders', 'contracts']);
  });

  it('leaves out the computed fields returning a table', async () => {
    const computedField = (name: string, functionName: string) => ({
      name,
      definition: { function: { schema: 'public', name: functionName } },
    });
    const { result } = renderHook(
      () =>
        usePermissionTables({
          source: source as never,
          table: table('users'),
          metadata: {
            version: 3,
            sources: [
              {
                ...source,
                tables: [
                  {
                    table: table('users'),
                    computed_fields: [
                      computedField('full_name', 'user_full_name'),
                      computedField('orders', 'user_orders'),
                    ],
                  },
                ],
              },
            ],
          } as never,
        }),
      { wrapper: testWrapper },
    );

    await waitFor(() =>
      expect(
        result.current.tables[0]?.computedFields.map(({ name }) => name),
      ).toEqual(['full_name']),
    );
  });
});
