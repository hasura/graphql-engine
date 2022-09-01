import { Database, Feature } from '..';
import { NetworkArgs, runSQL } from '../api';
import { adaptIntrospectedTables } from '../common/utils';
import { getTableColumns } from './introspection';

export type MssqlTable = { schema: string; name: string };

export const mssql: Database = {
  introspection: {
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables: async ({
      dataSourceName,
      httpClient,
    }: { dataSourceName: string } & NetworkArgs) => {
      const sql = `
      select table_name, table_schema, table_type
      from information_schema.tables
      where table_schema not in (
        'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
      );
      `;

      const tables = await runSQL({
        source: {
          name: dataSourceName,
          kind: 'mssql',
        },
        sql,
        httpClient,
      });

      return adaptIntrospectedTables(tables);
    },
    getDatabaseHierarchy: async () => {
      return ['schema', 'name'];
    },
    getTableColumns,
  },
};
