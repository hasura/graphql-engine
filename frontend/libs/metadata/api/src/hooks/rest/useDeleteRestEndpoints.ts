import { useMetadata, useMetadataMigration } from '../metadata';
import { removeOperationsFromQueryCollectionMetadataArgs } from '../queryCollections';

export const useDeleteRestEndpoints = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { data: metadata } = useMetadata();

  const deleteRestEndpoints = (
    endpoints: string[],
    options?: Parameters<typeof mutate>[1],
  ) => {
    return mutate(
      {
        query: {
          type: 'bulk',
          resource_version: metadata?.resource_version,
          args: [
            ...endpoints.map((endpoint) => ({
              type: 'drop_rest_endpoint' as const,
              args: {
                name: endpoint,
              },
            })),
            ...removeOperationsFromQueryCollectionMetadataArgs(
              'allowed-queries',
              endpoints,
            ),
          ],
        },
      },
      options,
    );
  };

  return { deleteRestEndpoints, ...rest };
};
