import { FrequentlyUsedColumn } from '../../types';
import { PostgresTable } from '../../postgres/types';
import { commonFrequentlyUsedColumns } from '../../postgres/config/getFrequentlyUsedColumns';

/**
 * CockroachDB variant of the Postgres presets. Triggers need CockroachDB
 * v24.3+, and the Postgres `updated_at` trigger SQL is adapted because:
 * - PL/pgSQL `record` variables are unimplemented, so `NEW` is set directly;
 * - `COMMENT ON TRIGGER` is not supported;
 * - v24.3 cannot `CREATE OR REPLACE` a function an active trigger uses, so the
 *   function is named per table rather than shared across a schema.
 */
export const getFrequentlyUsedColumns = (): FrequentlyUsedColumn[] => [
  ...commonFrequentlyUsedColumns,
  {
    name: 'updated_at',
    validFor: ['add', 'modify'],
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
    defaultText: 'now() + trigger to set value on update',
    dependentSQLGenerator: (table, columnName) => {
      const { schema: schemaName, name: tableName } = table as PostgresTable;
      const functionName = `set_current_timestamp_${tableName}_${columnName}`;
      const triggerName = `set_${schemaName}_${tableName}_${columnName}`;
      const upSql = `
CREATE OR REPLACE FUNCTION "${schemaName}"."${functionName}"()
RETURNS TRIGGER AS $$
BEGIN
  NEW."${columnName}" := NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER "${triggerName}"
BEFORE UPDATE ON "${schemaName}"."${tableName}"
FOR EACH ROW
EXECUTE FUNCTION "${schemaName}"."${functionName}"();
`;

      const downSql = `DROP TRIGGER IF EXISTS "${triggerName}" ON "${schemaName}"."${tableName}";`;

      return {
        upSql,
        downSql,
      };
    },
  },
];
