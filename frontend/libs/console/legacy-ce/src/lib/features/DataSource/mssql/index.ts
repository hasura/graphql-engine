import { Table } from '../../hasura-metadata-types';
import { Database, Feature, GetVersionProps } from '..';
import { NetworkArgs, runSQL } from '../api';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
import { adaptIntrospectedTables } from '../common/utils';
import {
  getTableColumns,
  getFKRelationships,
  getTablesListAsTree,
  getSupportedOperators,
} from './introspection';
import { getTableRows } from './query';
import { postgresCapabilities } from '../common/capabilities';

export type MssqlTable = { schema: string; name: string };

export const mssql: Database = {
  ...defaultDatabaseProps,
  introspection: {
    getVersion: async ({ dataSourceName, httpClient }: GetVersionProps) => {
      const result = await runSQL({
        source: {
          name: dataSourceName,
          kind: 'mssql',
        },
        sql: `SELECT @@VERSION as version;`,
        httpClient,
      });
      return result.result?.[1][0] ?? '';
    },
    getDriverInfo: async () => ({
      name: 'mssql',
      displayName: 'MS SQL Server',
      release: 'GA',
    }),
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
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
    getFKRelationships,
    getTablesListAsTree,
    getSupportedOperators,
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, schema } = table as MssqlTable;
      return schema === 'dbo' ? name : `${schema}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select', 'insert', 'update', 'delete'];
    },
  },
};
