import { Database, Feature } from '../index';

export const gdc: Database = {
  connectDB: {
    getConfigSchema: async () => {
      return Feature.NotImplemented;
    },
  },
  introspectTables: async () => {
    return Feature.NotImplemented;
  },
};
