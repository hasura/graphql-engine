import { Database, Feature } from '..';

export const bigquery: Database = {
  connectDB: {
    getConfigSchema: async () => {
      return Feature.NotImplemented;
    },
  },
};
