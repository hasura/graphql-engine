import { Database, Feature } from '../index';

export const gdc: Database = {
  introspection: {
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables: async () => {
      return Feature.NotImplemented;
    },
  },
};
