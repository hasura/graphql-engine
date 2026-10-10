import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { deleteInheritedRoleQuery } from './useDropInheritedRole';
import { addInheritedRoleQuery } from './useAddInheritedRole';
import { useMetadataMigration } from '../metadata';
import { useErrorNotification } from '../notification';

export const useUpdateInheritedRole = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  return useCallback(
    async (roleName: string, roleSet: string[]) => {
      mutation.mutate(
        {
          query: {
            type: 'bulk',
            args: [
              deleteInheritedRoleQuery(roleName),
              addInheritedRoleQuery(roleName, roleSet),
            ],
          },
        },
        {
          onSuccess: () => {
            hasuraToast({
              title: 'Updated inherited role!',
              type: 'success',
            });
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Updating inherited role failed!',
              error: err,
            });
          },
        },
      );
    },
    [mutation, showErrorNotification],
  );
};
