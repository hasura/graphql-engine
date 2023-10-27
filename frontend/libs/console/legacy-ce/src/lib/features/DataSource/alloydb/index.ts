import { Table } from '../../hasura-metadata-types';
import {
  defaultDatabaseProps,
  defaultIntrospectionProps,
} from '../common/defaultDatabaseProps';
import { Database, GetVersionProps } from '..';
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
import { getIsTableView } from './introspection/getIsTableView';
import { consoleDataTypeToSQLTypeMap, consoleScalars } from '../postgres/utils';

export type AlloyDbTable = { name: string; schema: string };

export const alloy: Database = {
  ...defaultDatabaseProps,
  introspection: {
    ...defaultIntrospectionProps,
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
    getIsTableView,
    getSupportedDataTypes: async () => consoleDataTypeToSQLTypeMap,
    getSupportedScalars: async () => consoleScalars,
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
