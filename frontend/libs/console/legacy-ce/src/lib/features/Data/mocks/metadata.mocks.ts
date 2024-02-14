import produce from 'immer';

import { allowedMetadataTypes } from '../../MetadataAPI';
import { Metadata } from '../../hasura-metadata-types';
import { MetadataReducer } from '../../../mocks/actions';

export const dataInitialData: Partial<Metadata['metadata']> = {
  sources: [
    {
      name: 'default',
      kind: 'postgres',
      tables: [
        {
          table: {
            name: 'user',
            schema: 'public',
          },
        },
      ],
      configuration: {
        connection_info: {
          database_url: {
            from_env: 'HASURA_GRAPHQL_DATABASE_URL',
          },
          isolation_level: 'read-committed',
          pool_settings: {
            connection_lifetime: 600,
            idle_timeout: 180,
            max_connections: 50,
            retries: 1,
          },
          use_prepared_statements: true,
        },
      },
    },
  ],
};

export const metadataHandlers: Partial<
  Record<allowedMetadataTypes, MetadataReducer>
> = {
  pg_update_source: (state, action) => {
    const { name, configuration } = action.args as {
      name: string;
      configuration: Record<string, unknown>;
    };
    const existingSource = state.metadata.sources.find(s => s.name === name);
    if (!existingSource) {
      return {
        status: 400,
        error: {
          path: '$.args.name',
          error: `source with name "${name}" does not exist`,
          code: 'not-exists',
        },
      };
    }
    return produce(state, draft => {
      const source = draft.metadata.sources.find(s => s.name === name);
      if (source?.configuration) {
        source.configuration = {
          ...source?.configuration,
          ...configuration,
        };
      }
    });
  },
};
