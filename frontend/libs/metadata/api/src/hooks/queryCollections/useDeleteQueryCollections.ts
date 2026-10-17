import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadata, useMetadataMigrationMutation } from '../metadata';

export type DeleteQueryCollectionArgs = {
  // Name of the query collection.
  collection: string;
  // When set to true, the collection (if present) is removed from the allowlist
  cascade?: boolean;
};

export const useDeleteQueryCollections = () => {
  const { data: metadata } = useMetadata();

  return useMetadataMigrationMutation(
    async (args: DeleteQueryCollectionArgs) => ({
      resource_version: metadata?.resource_version,
      type: 'drop_query_collection',
      args: args,
    }),
    {
      onSuccess: (_, variables) => {
        hasuraToast({
          type: 'success',
          title: 'Collection deleted',
          message: `Query collection "${variables.collection}" deleted successfully`,
        });
      },
      onError: (error) => {
        showErrorNotification({
          title: 'Error deleting collection',
          error,
        });
      },
    },
  );
};
