import { Database, Feature } from '..';
import { runSQL } from '../api';
import { adaptIntrospectedTables } from '../utils';

export const mssql: Database = {
  connectDB: {
    getConfigSchema: async () => {
      return Feature.NotImplemented;
    },
  },
  introspectTables: async (dataSourceName: string) => {
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
    });

    return adaptIntrospectedTables(tables);
  },
};
