import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '@hasura/metadata/api';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';

export type RemoveActionPermissionArgs = {
  role: string;
  action: string;
};

export const getDropActionPermissionQuery = (
  args: RemoveActionPermissionArgs,
) => {
  return {
    type: 'drop_action_permission' as const,
    args,
  };
};

const useDropActionPermission = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const dropActionPermission = async (
    args: RemoveActionPermissionArgs,
    options?: MetadataMigrationOptions,
  ) => {
    return mutate(
      {
        query: getDropActionPermissionQuery(args),
      },
      {
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Permission removed successfully!',
            type: 'success',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (err) => {
          showErrorNotification({
            title: 'Removing permission failed!',
            error: err,
          });
        },
      },
    );
  };

  return {
    dropActionPermission,
    ...rest,
  };
};

export default useDropActionPermission;
