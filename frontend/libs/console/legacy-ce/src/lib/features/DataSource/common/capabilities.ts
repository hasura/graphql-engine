import { Capabilities } from '@hasura/dc-api-types';

export const postgresCapabilities: Capabilities = {
  mutations: {
    insert: {},
    update: {},
    delete: {},
  },
  queries: {},
};
