import { Table } from '../../hasura-metadata-types';
import { Database, Feature } from '..';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
import {
  getTrackableTables,
  getTableColumns,
  getTablesListAsTree,
  getSupportedOperators,
} from './introspection';
import { getTableRows } from './query';
import { Capabilities } from '@hasura/dc-api-types';

export type BigQueryTable = { name: string; dataset: string };

export const bigquery: Database = {
  ...defaultDatabaseProps,
  introspection: {
    getDriverInfo: async () => ({
      name: 'bigquery',
      displayName: 'BigQuery',
      release: 'GA',
    }),
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getDriverCapabilities: async () => {
      const bigQueryCapabilities: Capabilities = {
        queries: {},
      };
      return Promise.resolve(bigQueryCapabilities);
    },
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['dataset', 'name'];
    },
    getTableColumns,
    getFKRelationships: async () => Feature.NotImplemented,
    getTablesListAsTree,
    getSupportedOperators,
    getDatabaseSchemas: async () => Feature.NotImplemented,
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, dataset } = table as BigQueryTable;
      return `${dataset}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select'];
    },
  },
};
