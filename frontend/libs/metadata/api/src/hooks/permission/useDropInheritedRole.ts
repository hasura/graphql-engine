import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata';
import { useErrorNotification } from '../notification';

export const deleteInheritedRoleQuery = (roleName: string) => ({
  type: 'drop_inherited_role' as const,
  args: {
    role_name: roleName,
  },
});

export const useDropInheritedRole = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  return useCallback(
    async (roleName: string) => {
      mutation.mutate(
        {
          query: deleteInheritedRoleQuery(roleName),
        },
        {
          onSuccess: () => {
            hasuraToast({
              title: 'Deleted inherited role!',
              type: 'success',
            });
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Deleting inherited role failed!',
              error: err,
            });
          },
        },
      );
    },
    [mutation, showErrorNotification],
  );
};
