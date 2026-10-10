import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata';
import { useErrorNotification } from '../notification';

export const addInheritedRoleQuery = (roleName: string, roleSet: string[]) => ({
  type: 'add_inherited_role' as const,
  args: {
    role_name: roleName,
    role_set: roleSet,
  },
});

export const useAddInheritedRole = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  return useCallback(
    async (roleName: string, roleSet: string[]) => {
      mutation.mutate(
        {
          query: addInheritedRoleQuery(roleName, roleSet),
        },
        {
          onSuccess: () => {
            hasuraToast({
              title: 'Added inherited role!',
              type: 'success',
            });
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Adding inherited role failed!',
              error: err,
            });
          },
        },
      );
    },
    [mutation],
  );
};
