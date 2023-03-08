import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';

export const useAddToAllowList = () => {
  const { data: metadata } = useMetadata();
  const { mutate, isSuccess, isLoading, error } = useMetadataMigration({});

  const addToAllowList = async (
    name: string,
    options?: Parameters<typeof mutate>[1]
  ) => {
    mutate(
      {
        query: {
          ...(metadata?.resource_version && {
            resource_version: metadata.resource_version,
          }),
          type: 'add_collection_to_allowlist',
          args: {
            collection: name,
          },
        },
      },
      options
    );
  };

  return { addToAllowList, isSuccess, isLoading, error };
};
