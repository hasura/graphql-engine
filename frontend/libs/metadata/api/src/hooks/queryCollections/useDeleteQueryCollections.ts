import { useMetadata, useMetadataMigration } from '../metadata';

export const useDeleteQueryCollections = () => {
  const { data: metadata } = useMetadata();
  const { mutate, ...rest } = useMetadataMigration();

  const deleteQueryCollection = async (
    name: string,
    options?: Parameters<typeof mutate>[1],
  ) => {
    return mutate(
      {
        query: {
          resource_version: metadata?.resource_version,
          type: 'drop_query_collection',
          args: {
            collection: name,
            cascade: true,
          },
        },
      },
      options,
    );
  };

  return { deleteQueryCollection, ...rest };
};
