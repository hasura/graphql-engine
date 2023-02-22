import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';

export const useDeleteQueryCollections = () => {
  const { data: metadata } = useMetadata();
  const { mutate, isSuccess, isLoading, error } = useMetadataMigration({});

  const deleteQueryCollection = async (
    name: string,
    options?: Parameters<typeof mutate>[1]
  ) => {
    mutate(
      {
        query: {
          ...(metadata?.resource_version && {
            resource_version: metadata.resource_version,
          }),
          type: 'drop_query_collection',
          args: {
            collection: name,
            cascade: true,
          },
        },
      },
      options
    );
  };

  return { deleteQueryCollection, isSuccess, isLoading, error };
};
