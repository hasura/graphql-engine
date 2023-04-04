import { Table } from '../../hasura-metadata-types';
import { Database, GetDefaultQueryRootProps, GetVersionProps } from '..';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
  getFKRelationships,
  getTablesListAsTree,
  getSupportedOperators,
} from './introspection';
import { getTableRows } from './query';
import { runSQL } from '../api';
import { postgresCapabilities } from '../common/capabilities';

export type PostgresTable = { name: string; schema: string };

export const postgres: Database = {
  ...defaultDatabaseProps,
  introspection: {
    getVersion: async ({ dataSourceName, httpClient }: GetVersionProps) => {
      const result = await runSQL({
        source: {
          name: dataSourceName,
          kind: 'postgres',
        },
        sql: `SELECT VERSION()`,
        httpClient,
      });
      return result.result?.[1][0] ?? '';
    },
    getDriverInfo: async () => ({
      name: 'postgres',
      displayName: 'Postgres',
      release: 'GA',
    }),
    getDatabaseConfiguration,
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
    getTrackableTables,
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
  modify: {
    defaultQueryRoot: async ({ table }: GetDefaultQueryRootProps) => {
      const { name, schema } = table as PostgresTable;

      return schema === 'public' ? name : `${schema}_${name}`;
    },
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, schema } = table as PostgresTable;
      return schema === 'public' ? name : `${schema}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select', 'insert', 'update', 'delete'];
    },
  },
};
