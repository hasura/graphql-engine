import { ConnectionSet } from '../../../../../../../metadata/types';
import {
  useMetadata,
  useMetadataMigration,
} from '../../../../../../MetadataAPI';

type ConnectionTemplateTestArgs = {
  request_context: {
    headers?: {
      [key: string]: string;
    };
    session?: {
      [key: string]: string;
    };
    query: {
      operation_type: string;
      operation_name?: string;
    };
  };
} & (
  | {
      connection_template: {
        template: string;
      };
    }
  | {
      source_name: string;
    }
);
export const useDynamicDbRouting = ({ sourceName }: { sourceName: string }) => {
  const { data, isLoading: isMetadaLoading } = useMetadata();

  const { mutate, isLoading } = useMetadataMigration({});

  const source = data?.metadata?.sources.find(
    source => source.name === sourceName
  );

  const connectionTemplate =
    source?.configuration?.connection_template?.template || null;
  const connectionSet = source?.configuration?.connection_set || [];

  const addConnection = async (
    connection: ConnectionSet,
    options?: Parameters<typeof mutate>[1]
  ): Promise<void> => {
    mutate(
      {
        query: {
          ...(data?.resource_version && {
            resource_version: data.resource_version,
          }),
          type: 'pg_update_source',
          args: {
            ...source,
            configuration: {
              ...source?.configuration,
              connection_set: [...connectionSet, connection],
            },
          },
        },
      },
      options
    );
  };

  const removeConnection = async (
    connectionName: string,
    options?: Parameters<typeof mutate>[1]
  ): Promise<void> => {
    mutate(
      {
        query: {
          ...(data?.resource_version && {
            resource_version: data.resource_version,
          }),
          type: 'pg_update_source',
          args: {
            ...source,
            configuration: {
              ...source?.configuration,
              connection_set:
                connectionSet.length === 1
                  ? null
                  : connectionSet.filter(c => c.name !== connectionName),
            },
          },
        },
      },
      options
    );
  };

  const updateConnection = async (
    connectionName: string,
    connection: ConnectionSet,
    options?: Parameters<typeof mutate>[1]
  ): Promise<void> => {
    mutate(
      {
        query: {
          ...(data?.resource_version && {
            resource_version: data.resource_version,
          }),
          type: 'pg_update_source',
          args: {
            ...source,
            configuration: {
              ...source?.configuration,
              connection_set: connectionSet.map(c =>
                c.name === connectionName ? connection : c
              ),
            },
          },
        },
      },
      options
    );
  };

  const updateConnectionTemplate = async (
    connectionTemplate?: string | null,
    options?: Parameters<typeof mutate>[1]
  ): Promise<void> => {
    mutate(
      {
        query: {
          ...(data?.resource_version && {
            resource_version: data.resource_version,
          }),
          type: 'pg_update_source',
          args: {
            ...source,
            configuration: {
              ...source?.configuration,
              connection_template: connectionTemplate
                ? { template: connectionTemplate }
                : null,
            },
          },
        },
      },
      options
    );
  };

  const testConnectionTemplate = async (
    args: ConnectionTemplateTestArgs,
    options?: Parameters<typeof mutate>[1]
  ) => {
    await mutate(
      {
        query: {
          ...(data?.resource_version && {
            resource_version: data.resource_version,
          }),
          type: 'pg_test_connection_template',
          args,
        },
      },
      options
    );
  };

  return {
    connectionTemplate,
    connectionSet,
    addConnection,
    removeConnection,
    updateConnection,
    updateConnectionTemplate,
    isLoading,
    isMetadaLoading,
    testConnectionTemplate,
  };
};
