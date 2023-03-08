import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';

export const useRemoveFromAllowList = () => {
  const { data: metadata } = useMetadata();
  const { mutate, isSuccess, isLoading, error } = useMetadataMigration({});

  const removeFromAllowList = async (
    name: string,
    options?: Parameters<typeof mutate>[1]
  ) => {
    mutate(
      {
        query: {
          ...(metadata?.resource_version && {
            resource_version: metadata.resource_version,
          }),
          type: 'drop_collection_from_allowlist',
          args: {
            collection: name,
          },
        },
      },
      options
    );
  };

  return { removeFromAllowList, isSuccess, isLoading, error };
};
