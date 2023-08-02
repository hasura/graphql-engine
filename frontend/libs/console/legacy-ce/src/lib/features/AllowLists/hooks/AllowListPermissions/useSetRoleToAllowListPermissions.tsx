import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '../../../MetadataAPI';

export const useSetRoleToAllowListPermission = (collectionName: string) => {
  const { mutate } = useMetadataMigration();

  const setRoleToAllowListPermission = (
    roles: string[],
    options?: Parameters<typeof mutate>[1]
  ): void => {
    const type: allowedMetadataTypes =
      'update_scope_of_collection_in_allowlist';
    mutate(
      {
        query: {
          type,
          args: {
            collection: collectionName,
            scope:
              roles.length === 0
                ? { global: true }
                : {
                    global: false,
                    roles,
                  },
          },
        },
      },
      options
    );
  };

  return { setRoleToAllowListPermission };
};
