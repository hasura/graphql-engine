import { runSQL } from '@hasura/metadata/api';
import { adaptIntrospectedTables } from '../../common/utils';
import { GetTrackableTablesProps } from '../../types';

export const getTrackableTables = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetTrackableTablesProps) => {
  const sql = `
      select table_name, table_schema, table_type
      from information_schema.tables
      where table_schema not in (
        'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
      );
      `;

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'mssql',
      },
      sql,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptIntrospectedTables(tables);
};
