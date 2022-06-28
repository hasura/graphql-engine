import { Database, Feature } from '..';

export const mssql: Database = {
  connectDB: {
    getConfigSchema: async () => {
      return Feature.NotImplemented;
    },
  },
};
