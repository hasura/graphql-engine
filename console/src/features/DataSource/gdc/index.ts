import { Database, Feature } from '../index';

export const gdc: Database = {
  introspection: {
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables: async () => {
      return Feature.NotImplemented;
    },
    getDatabaseHierarchy: async () => {
      /**
       * Once we have the API for fetching the hierarchy info via HGE, we can add that logic here
       */
      return Feature.NotImplemented;
    },
    getTableColumns: async () => {
      return Feature.NotImplemented;
    },
  },
};
