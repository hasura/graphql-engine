import { runDatabaseMigration } from '@hasura/metadata/api';
import type { TableColumn } from '../../types';
import {
  getDeleteRowSQL,
  getInsertRowSQL,
  insertRows,
  toSQLLiteral,
} from './insertRows';

vi.mock('@hasura/metadata/api', () => ({
  runDatabaseMigration: vi.fn(() => Promise.resolve([])),
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

const endpoints = { queryV2: '/v2/query' } as never;

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

  beforeEach(() => vi.mocked(runDatabaseMigration).mockClear());

  it('does not request the inserted rows nor create a migration by default', async () => {
    const fetchJson = vi.fn().mockResolvedValue({ affected_rows: 1 });

    const affectedRows = await insertRows({
      endpoints,
      fetchJson,
      args: {
        source,
        table,
        objects: [{ note: 'x' }],
        columns,
        defaultColumns: [],
      },
    });

    expect(affectedRows).toBe(1);
    const body = JSON.parse(fetchJson.mock.calls[0][1].body);
    expect(body.args.returning).toBeUndefined();
    expect(runDatabaseMigration).not.toHaveBeenCalled();
  });

  it('writes a migration for the inserted row without executing it again', async () => {
    const fetchJson = vi.fn().mockResolvedValue({
      affected_rows: 1,
      returning: [{ id: 7, note: 'x' }],
    });

    await insertRows({
      endpoints,
      fetchJson,
      args: {
        source,
        table,
        objects: [{ note: 'x' }],
        columns,
        defaultColumns: [],
        isMigration: true,
      },
    });

    const body = JSON.parse(fetchJson.mock.calls[0][1].body);
    expect(body.args.returning).toEqual(['id', 'note']);
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
    const fetchJson = vi.fn().mockResolvedValue({
      affected_rows: 1,
      returning: [{ id: 7, note: 'x' }],
    });

    await expect(
      insertRows({
        endpoints,
        fetchJson,
        args: {
          source,
          table,
          objects: [{ note: 'x' }],
          columns,
          defaultColumns: [],
          isMigration: true,
        },
      }),
    ).rejects.toThrow(
      'The row was inserted, but creating the migration failed: CLI server unreachable',
    );
  });
});
