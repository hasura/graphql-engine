import { runDatabaseMigration } from '@hasura/metadata/api';
import type { TableColumn } from '../../types';
import { runRowMutationFromGraphQL } from '../../common/modify/runRowMutation';
import {
  getDeleteRowSQL,
  getInsertRowSQL,
  insertRows,
  toSQLLiteral,
} from './insertRows';

vi.mock('@hasura/metadata/api', () => ({
  runDatabaseMigration: vi.fn(() => Promise.resolve([])),
}));

vi.mock('../../common/modify/runRowMutation', () => ({
  runRowMutationFromGraphQL: vi.fn(),
}));

const table = { schema: 'public', name: 'orders' };

const columns: TableColumn[] = [
  {
    name: 'id',
    dataType: 'integer',
    consoleDataType: 'integer',
    isPrimaryKey: true,
  },
  { name: 'note', dataType: 'text', consoleDataType: 'text' },
];

const endpoints = { graphQLUrl: '/v1/graphql' } as never;

describe('toSQLLiteral', () => {
  it.each`
    value                   | expected
    ${null}                 | ${'NULL'}
    ${42}                   | ${'42'}
    ${true}                 | ${'true'}
    ${"it's"}               | ${"'it''s'"}
    ${{ a: "b'c" }}         | ${`'{"a":"b''c"}'`}
    ${[1, 2]}               | ${"'{1,2}'"}
    ${['a b', 'x"y', null]} | ${`'{"a b","x\\"y",NULL}'`}
  `('converts $value', ({ value, expected }) => {
    expect(toSQLLiteral(value)).toBe(expected);
  });
});

describe('insert migration SQL', () => {
  const row = { id: 1, note: "O'Brien" };

  it('inserts the returned row', () => {
    expect(getInsertRowSQL(table, row)).toBe(
      `INSERT INTO "public"."orders" ("id", "note") VALUES (1, 'O''Brien');`,
    );
  });

  it('deletes the row by its primary key', () => {
    expect(getDeleteRowSQL(table, row, columns)).toBe(
      `DELETE FROM "public"."orders" WHERE "id" = 1;`,
    );
  });

  it('deletes the row by all its values when there is no primary key', () => {
    expect(
      getDeleteRowSQL(
        table,
        { id: 1, note: null },
        columns.map((c) => ({ ...c, isPrimaryKey: false })),
      ),
    ).toBe(`DELETE FROM "public"."orders" WHERE "id" = 1 AND "note" IS NULL;`);
  });
});

describe('insertRows', () => {
  const source = { name: 'default', kind: 'postgres' } as never;

  const fetchJson = vi.fn();
  const insertedRow = { affectedRows: 1, returning: [{ id: 7, note: 'x' }] };

  beforeEach(() => {
    vi.mocked(runDatabaseMigration).mockClear();
    vi.mocked(runRowMutationFromGraphQL).mockReset();
  });

  it('inserts through GraphQL without returning rows nor creating a migration by default', async () => {
    vi.mocked(runRowMutationFromGraphQL).mockResolvedValue({
      affectedRows: 1,
      returning: [],
    });

    const affectedRows = await insertRows({
      endpoints,
      fetchJson,
      args: { source, table, objects: [{ note: 'x' }], columns },
    });

    expect(affectedRows).toBe(1);
    expect(runRowMutationFromGraphQL).toHaveBeenCalledWith(
      expect.objectContaining({
        operation: 'insert',
        table,
        objects: [{ note: 'x' }],
        returning: undefined,
      }),
    );
    expect(runDatabaseMigration).not.toHaveBeenCalled();
  });

  it('writes a migration for the inserted row without executing it again', async () => {
    vi.mocked(runRowMutationFromGraphQL).mockResolvedValue(insertedRow);

    await insertRows({
      endpoints,
      fetchJson,
      args: {
        source,
        table,
        objects: [{ note: 'x' }],
        columns,
        isMigration: true,
      },
    });

    expect(runRowMutationFromGraphQL).toHaveBeenCalledWith(
      expect.objectContaining({ returning: ['id', 'note'] }),
    );
    expect(runDatabaseMigration).toHaveBeenCalledWith({
      endpoints,
      fetchJson,
      isMigration: true,
      args: {
        name: 'insert_into_public_orders',
        source: { name: 'default', kind: 'postgres' },
        up: [
          {
            sql: `INSERT INTO "public"."orders" ("id", "note") VALUES (7, 'x');`,
          },
        ],
        down: [{ sql: `DELETE FROM "public"."orders" WHERE "id" = 7;` }],
        skip_execution: true,
      },
    });
  });

  it('reports that the row was inserted when creating the migration fails', async () => {
    vi.mocked(runDatabaseMigration).mockRejectedValueOnce(
      new Error('CLI server unreachable'),
    );
    vi.mocked(runRowMutationFromGraphQL).mockResolvedValue(insertedRow);

    await expect(
      insertRows({
        endpoints,
        fetchJson,
        args: {
          source,
          table,
          objects: [{ note: 'x' }],
          columns,
          isMigration: true,
        },
      }),
    ).rejects.toThrow(
      'The row was inserted, but creating the migration failed: CLI server unreachable',
    );
  });
});
