import { MetadataResponse } from '../../../../MetadataAPI';

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
          ],
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

export const metadata_with_no_query_collections: MetadataResponse = {
  resource_version: 48,
  metadata: {
    version: 3,
    sources: [],
    remote_schemas: [],
    inherited_roles: [],
  },
};
