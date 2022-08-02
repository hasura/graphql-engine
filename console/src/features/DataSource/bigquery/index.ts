import { Database, Feature } from '..';
import { getTrackableTables } from './introspection';

export const bigquery: Database = {
  introspection: {
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables,
  },
};
