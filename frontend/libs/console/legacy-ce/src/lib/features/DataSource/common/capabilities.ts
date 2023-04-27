import { DriverCapability } from '../types';

export const postgresCapabilities: DriverCapability = {
  mutations: {
    insert: {},
    update: {},
    delete: {},
  },
  queries: {},
  functions: {},
  data_schema: {
    supports_foreign_keys: true,
  },
};
