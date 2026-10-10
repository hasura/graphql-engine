import { runSQLBulk } from '@hasura/metadata/api';
import { GetTrackableTablesProps } from '../../types';
import { adaptIntrospectedTables } from '../utils';

export const getTrackableTables = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetTrackableTablesProps) => {
  const partitionSQL = `SELECT
  child.relname       AS partition
FROM pg_inherits
  JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
  JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace`;

  const sql = `
  SELECT
    info_schema.table_name,
    info_schema.table_schema,
    info_schema.table_type
  FROM information_schema.tables as info_schema
  WHERE
    info_schema.table_schema NOT IN ('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal')
  UNION
  SELECT
    matviewname AS table_name,
    schemaname AS table_schema,
    'MATERIALIZED VIEW' AS table_type
  FROM pg_matviews
  WHERE schemaname NOT in('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal')
  `;

  const source = {
    name: dataSourceName,
    kind: 'postgres' as const,
  };

  const result = await runSQLBulk({
    args: [
      {
        sql,
        source,
        readOnly: true,
      },
      {
        sql: partitionSQL,
        source,
        readOnly: true,
      },
    ],
    fetchJson,
    url: endpoints.queryV2,
    type: 'concurrent_bulk',
  });

  return adaptIntrospectedTables([result[0], result[1]]);
};
