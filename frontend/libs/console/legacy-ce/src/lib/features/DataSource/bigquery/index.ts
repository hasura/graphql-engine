import { Database, Feature } from '..';
import {
  getTrackableTables,
  getTableColumns,
  getTablesListAsTree,
  getSupportedOperators,
} from './introspection';
import { getTableRows } from './query';

export type BigQueryTable = { name: string; dataset: string };

export const bigquery: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'bigquery',
      displayName: 'BigQuery',
      release: 'GA',
    }),
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['dataset', 'name'];
    },
    getTableColumns,
    getFKRelationships: async () => Feature.NotImplemented,
    getTablesListAsTree,
    getSupportedOperators,
  },
  query: {
    getTableRows,
  },
};
