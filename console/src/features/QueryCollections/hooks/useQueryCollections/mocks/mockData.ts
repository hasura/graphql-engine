import { MetadataResponse } from '@/features/MetadataAPI';
import { ServerConfig } from '@/hooks';
import { ResponseBodyMetadataTypeError } from '../../../../CronTriggers/components/Form/mocks/types';

export const createMetadata = (): MetadataResponse => ({
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [],
    remote_schemas: [],
    inherited_roles: [],
    allowlist: [
      {
        collection: 'allowed_queries',
        scope: {
          global: true,
        },
      },
      {
        collection: 'allowed_queries',
        scope: {
          global: false,
          roles: ['user'],
        },
      },
    ],
    query_collections: [
      { name: 'allowed_queries', definition: { queries: [] } },
      { name: 'other_queries', definition: { queries: [] } },
    ],
  },
});

export const createResponse: {
  type: string;
  version: number;
  args: Record<string, unknown>;
} = {
  type: 'export_metadata',
  version: 2,
  args: {},
};

export const alreadyExistsResponse = (
  name: string
): ResponseBodyMetadataTypeError => ({
  path: '$.args.name',
  error: `query collection with name "${name}" already exists`,
  code: 'already-exists',
});

export const notExistsResponse = (
  name: string
): ResponseBodyMetadataTypeError => ({
  path: '$.args.collection',
  error: `query collection with name "${name}" does not exist`,
  code: 'not-exists',
});

export const configApiEnabled: Partial<ServerConfig> = {
  is_allow_list_enabled: true,
};
