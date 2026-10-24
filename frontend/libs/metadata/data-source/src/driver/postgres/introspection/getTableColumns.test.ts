import { runSQL } from '@hasura/metadata/api';
import { getTableColumnsCurry } from './getTableColumns';

vi.mock('@hasura/metadata/api', () => ({ runSQL: vi.fn() }));

vi.mock('../../common/utils', () => ({
  introspectTableScalarTypes: vi.fn(async () => ({
    scalarTypes: [],
    metadataTable: { table: { schema: 'public', name: 'album' } },
  })),
}));

const result = (rows: string[][]) => ({
  result_type: 'TuplesOk' as const,
  result: rows,
});

describe('getTableColumnsCurry', () => {
  beforeEach(() => {
    vi.mocked(runSQL).mockImplementation(async ({ args }) => {
      if (args.sql.includes('indisprimary'))
        return result([['attname'], ['id']]);
      if (args.sql.includes('nextval')) return result([['column_name']]);
      return result([
        ['column_name', 'data_type', 'is_nullable', 'sql_type', 'default'],
        ['id', 'uuid', 'NO', 'uuid', 'gen_random_uuid()'],
        // CockroachDB reports a column without a default as `NULL`
        ['title', 'text', 'YES', 'text', 'NULL'],
        ['scores', 'integer[]', 'YES', 'integer[]', 'NULL'],
      ]);
    });
  });

  it.each(['postgres', 'citus', 'cockroach'] as const)(
    'introspects %s columns with its own run_sql API',
    async (driver) => {
      const columns = await getTableColumnsCurry(driver)({
        dataSourceName: 'db',
        table: { schema: 'public', name: 'album' },
        endpoints: { queryV2: '/v2/query' } as never,
        fetchJson: vi.fn(),
      });

      vi.mocked(runSQL).mock.calls.forEach(([{ args }]) =>
        expect(args.source).toEqual({ name: 'db', kind: driver }),
      );
      expect(columns).toMatchObject([
        { name: 'id', isPrimaryKey: true, defaultValue: 'gen_random_uuid()' },
        { name: 'title', isPrimaryKey: false, defaultValue: null },
        { name: 'scores', consoleDataType: 'array', arrayItemType: 'integer' },
      ]);
      vi.mocked(runSQL).mockClear();
    },
  );
});
