import { useMetadata, useMetadataMigration } from '../metadata';

export const useRenameQueryCollection = () => {
  const { data: metadata } = useMetadata();
  const { mutate, ...rest } = useMetadataMigration();

  const renameQueryCollection = (
    name: string,
    newName: string,
    options?: Parameters<typeof mutate>[1],
  ) => {
    return mutate(
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
      options,
    );
  };

  return { renameQueryCollection, ...rest };
};
