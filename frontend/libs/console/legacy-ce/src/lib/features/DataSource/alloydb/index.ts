import { Table } from '../../hasura-metadata-types';
import { Database, GetVersionProps } from '..';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
  getFKRelationships,
  getTablesListAsTree,
  getSupportedOperators,
  getDatabaseSchemas,
} from '../postgres/introspection';
import { getTableRows } from '../postgres/query';
import { runSQL } from '../api';
import { postgresCapabilities } from '../common/capabilities';

export type AlloyDbTable = { name: string; schema: string };

export const alloy: Database = {
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
      console.log(result);
      return result.result?.[1][0] ?? '';
    },
    getDriverInfo: async () => ({
      name: 'alloy',
      displayName: 'AlloyDB',
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
    getDatabaseSchemas,
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, schema } = table as AlloyDbTable;
      return schema === 'public' ? name : `${schema}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select', 'insert', 'update', 'delete'];
    },
  },
};
