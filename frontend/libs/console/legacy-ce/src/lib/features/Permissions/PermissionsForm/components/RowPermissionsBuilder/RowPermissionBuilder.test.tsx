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

const Form = () => {
  const methods = useForm({ defaultValues: { filter: permissions } });
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
});
