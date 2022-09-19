import { Database, Feature } from '..';
import {
  getTrackableTables,
  getTableColumns,
  getTablesListAsTree,
} from './introspection';

export type BigQueryTable = { name: string; dataset: string };

export const bigquery: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'bigquery',
      displayName: 'Big Query',
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
  },
  query: {
    getTableRows: async () => Feature.NotImplemented,
  },
};
