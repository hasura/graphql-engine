import { MetadataResponse } from '@/features/MetadataAPI';

export const metadata: MetadataResponse = {
  resource_version: 48,
  metadata: {
    version: 3,
    sources: [],
    remote_schemas: [],
    inherited_roles: [],
    query_collections: [
      {
        name: 'allowed-queries',
        definition: {
          queries: [
            {
              name: 'MyQuery',
              query: 'query MyQuery {\n  user {\n    id\n  }\n}\n',
            },
            {
              name: 'MyQuery2',
              query: 'query MyQuery2 {\n  user {\n    id\n  }\n}\n',
            },
            {
              name: 'MyMutation',
              query: 'query MyMutation {\n  user {\n    id\n  }\n}\n',
            },
          ],
        },
      },
      {
        name: 'other-queries',
        definition: {
          queries: [],
        },
      },
    ],
    allowlist: [
      {
        collection: 'allowed-queries',
        scope: {
          global: true,
        },
      },
    ],
  },
};
