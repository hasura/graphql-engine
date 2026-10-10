import { runSQL } from '@hasura/metadata/api';
import { GetTrackableTablesProps } from '../../types';
import { adaptIntrospectedTables } from '../../common/utils';

export const getTrackableTables = async ({
  dataSourceName,
  fetchJson,
  endpoints,
}: GetTrackableTablesProps) => {
  const sql = `
      WITH partitions as (
        SELECT array(
          SELECT
          child.relname       AS partition
      FROM pg_inherits
          JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
          JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
        ) as names
      )
      SELECT info_schema.table_name, info_schema.table_schema, info_schema.table_type
      FROM information_schema.tables as info_schema, partitions
      WHERE
        info_schema.table_schema NOT IN ('pg_catalog', 'crdb_internal', 'information_schema', 'columnar', 'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog', '_timescaledb_internal', 'pg_extension')
        AND NOT (info_schema.table_name = ANY (partitions.names));
      `;
  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'cockroach',
      },
      sql,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptIntrospectedTables(tables);
};
