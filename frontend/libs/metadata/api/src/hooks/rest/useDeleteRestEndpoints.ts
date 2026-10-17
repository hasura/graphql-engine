import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadata, useMetadataMigrationMutation } from '../metadata';
import { removeOperationsFromQueryCollectionMetadataArgs } from '../queryCollections';

export const useDeleteRestEndpoints = () => {
  const { data: metadata } = useMetadata();

  return useMetadataMigrationMutation(
    async (endpoints: string[]) => ({
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
    }),
    {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          message: `Successfully deleted REST endpoint`,
        });
      },
      onError: (error) => {
        showErrorNotification({
          title: 'Error deleting REST endpoint',
          error,
        });
      },
    },
  );
};
