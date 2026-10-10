import { getFrequentlyUsedColumns } from './getFrequentlyUsedColumns';
import { getFrequentlyUsedColumns as getPostgresFrequentlyUsedColumns } from '../../postgres/config/getFrequentlyUsedColumns';

const table = { schema: 'public', name: 'orders' };

const getUpdatedAtSql = (
  columns: ReturnType<typeof getFrequentlyUsedColumns>,
) =>
  columns
    .find((c) => c.name === 'updated_at')
    ?.dependentSQLGenerator?.(table, 'updated_at');

describe('cockroach getFrequentlyUsedColumns', () => {
  it('offers the same presets as postgres', () => {
    expect(getFrequentlyUsedColumns().map((c) => [c.name, c.type])).toEqual(
      getPostgresFrequentlyUsedColumns().map((c) => [c.name, c.type]),
    );
  });

  it('generates CockroachDB-compatible updated_at trigger SQL', () => {
    const sql = getUpdatedAtSql(getFrequentlyUsedColumns());

    expect(sql?.upSql).toContain(
      'CREATE OR REPLACE FUNCTION "public"."set_current_timestamp_orders_updated_at"()',
    );
    expect(sql?.upSql).toContain('NEW."updated_at" := NOW();');
    expect(sql?.upSql).toContain(
      'EXECUTE FUNCTION "public"."set_current_timestamp_orders_updated_at"();',
    );
    expect(sql?.upSql).not.toMatch(/record/i);
    expect(sql?.upSql).not.toContain('COMMENT ON TRIGGER');
    expect(sql?.downSql).toBe(
      'DROP TRIGGER IF EXISTS "set_public_orders_updated_at" ON "public"."orders";',
    );
  });
});
