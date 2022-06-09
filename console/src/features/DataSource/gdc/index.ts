import { Database, Feature } from '../index';

export const gdc: Database = {
  connectDB: {
    getValidationSchema: async () => {
      return Feature.NotImplemented;
    },
    getUISchema: async () => {
      return Feature.NotImplemented;
    },
  },
};
