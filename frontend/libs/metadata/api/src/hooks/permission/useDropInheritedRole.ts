import { hasuraToast } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from '../metadata';
import { useErrorNotification } from '../notification';

export const deleteInheritedRoleQuery = (roleName: string) => ({
  type: 'drop_inherited_role' as const,
  args: {
    role_name: roleName,
  },
});

export const useDropInheritedRole = () => {
  const showErrorNotification = useErrorNotification();

  return useMetadataMigrationMutation(
    async (roleName: string) => deleteInheritedRoleQuery(roleName),
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
};
