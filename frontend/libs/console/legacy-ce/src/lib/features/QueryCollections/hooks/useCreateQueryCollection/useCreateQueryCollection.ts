import {
  MetadataResponse,
  useMetadata,
  useMetadataMigration,
} from '../../../MetadataAPI';

export const useCreateQueryCollection = () => {
  const { mutate, isSuccess, isLoading, error } = useMetadataMigration();
  const { data: metadata } = useMetadata();

  const createQueryCollection = async (
    name: string,
    options?: Parameters<typeof mutate>[1] & { addToAllowList?: boolean }
  ) => {
    const { addToAllowList = false, ...mutateOptions } = options || {};
    mutate(
      {
        query: {
          ...(metadata?.resource_version && {
            resource_version: metadata.resource_version,
          }),
          type: 'bulk',
          args: [
            {
              type: 'create_query_collection',
              args: {
                name,
                definition: {
                  queries: [],
                },
              },
            },
            addToAllowList
              ? {
                  type: 'add_collection_to_allowlist',
                  args: {
                    collection: name,
                  },
                }
              : undefined,
          ].filter(op => op),
        },
      },
      mutateOptions
    );
  };

  return { createQueryCollection, isSuccess, isLoading, error };
};

export const createAllowedQueriesIfNeeded = (
  queryCollection: string,
  metadata: MetadataResponse | undefined
) => {
  return queryCollection === 'allowed-queries' &&
    !metadata?.metadata?.query_collections?.find(
      q => q.name === queryCollection
    )
    ? [
        {
          type: 'create_query_collection',
          args: {
            name: queryCollection,
            definition: {
              queries: [],
            },
          },
        },
        {
          type: 'add_collection_to_allowlist',
          args: {
            collection: queryCollection,
          },
        },
      ]
    : [];
};
