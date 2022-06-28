import { Database, Feature } from '..';

export const citus: Database = {
  connectDB: {
    getConfigSchema: async () => {
      return Feature.NotImplemented;
    },
  },
};
