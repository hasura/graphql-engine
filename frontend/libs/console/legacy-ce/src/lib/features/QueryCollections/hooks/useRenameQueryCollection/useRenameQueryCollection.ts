import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';

export const useRenameQueryCollection = () => {
  const { data: metadata } = useMetadata();
  const { mutate, isSuccess, isLoading, error } = useMetadataMigration({});

  const renameQueryCollection = async (
    name: string,
    newName: string,
    options?: Parameters<typeof mutate>[1]
  ) => {
    mutate(
      {
        query: {
          ...(metadata?.resource_version && {
            resource_version: metadata.resource_version,
          }),
          type: 'rename_query_collection',
          args: {
            name,
            new_name: newName,
          },
        },
      },
      options
    );
  };

  return { renameQueryCollection, isSuccess, isLoading, error };
};
