import { Capabilities } from '@hasura/dc-api-types';

export const postgresCapabilities: Capabilities = {
  mutations: {
    insert: {},
    update: {},
    delete: {},
  },
  queries: {
    foreach: {},
  },
  relationships: {},
  user_defined_functions: {},
  data_schema: {
    supports_foreign_keys: true,
  },
  interpolated_queries: {},
};
