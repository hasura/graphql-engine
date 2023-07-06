import { useCallback } from 'react';
import { useMetadata, useMetadataMigration } from '../../MetadataAPI';
import { removeOperationsFromQueryCollectionMetadataArgs } from '../../QueryCollections/hooks';

export const useDeleteRestEndpoints = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata } = useMetadata();

  const deleteRestEndpoints = useCallback(
    (endpoints: string[], options?: Parameters<typeof mutate>[1]) => {
      return mutate(
        {
          query: {
            type: 'bulk',
            ...(metadata?.resource_version && {
              resource_version: metadata.resource_version,
            }),
            args: [
              ...endpoints.map(endpoint => ({
                type: 'drop_rest_endpoint',
                args: {
                  name: endpoint,
                },
              })),
              ...removeOperationsFromQueryCollectionMetadataArgs(
                'allowed-queries',
                endpoints
              ),
            ],
          },
        },
        {
          ...options,
        }
      );
    },
    [mutate, metadata]
  );

  return { deleteRestEndpoints, ...rest };
};
