import { useMetadataMigration } from '@hasura/metadata/api';

export const useSetRoleToAllowListPermission = (collectionName: string) => {
  const { mutate } = useMetadataMigration();

  const setRoleToAllowListPermission = (
    roles: string[],
    options?: Parameters<typeof mutate>[1],
  ): void => {
    const type = 'update_scope_of_collection_in_allowlist' as const;
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
      options,
    );
  };

  return { setRoleToAllowListPermission };
};
