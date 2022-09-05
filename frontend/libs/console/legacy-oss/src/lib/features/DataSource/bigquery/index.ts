import { Database, Feature } from '..';
import { getTrackableTables, getTableColumns } from './introspection';

export type BigQueryTable = { name: string; dataset: string };

export const bigquery: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'bigquery',
      displayName: 'Big Query',
      release: 'Beta',
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
  },
};
