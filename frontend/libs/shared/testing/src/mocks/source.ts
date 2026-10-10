import { AllowedMetadataTypes, Metadata } from '@hasura/shared/types';

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
          event_triggers: [],
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

export const sourceHandlers: Partial<Record<AllowedMetadataTypes, any>> = {
  pg_update_source: (state, action) => {
    const { name, configuration } = action.args as {
      name: string;
      configuration: Record<string, unknown>;
    };
    const existingSource = state.metadata.sources.find((s) => s.name === name);
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
    if (!existingSource.configuration) {
      return state;
    }
    return {
      ...state,
      metadata: {
        ...state.metadata,
        sources: state.metadata.sources.map((s) =>
          s.name === name
            ? {
                ...s,
                configuration: {
                  ...(s.configuration as Record<string, unknown>),
                  ...configuration,
                },
              }
            : s,
        ),
      },
    };
  },
};
