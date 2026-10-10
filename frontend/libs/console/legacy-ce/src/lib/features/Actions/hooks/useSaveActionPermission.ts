import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '@hasura/metadata/api';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';

export type SaveActionPermissionArgs = {
  action: string;
  role: string;
};

export const getCreateActionPermissionQuery = (
  args: SaveActionPermissionArgs,
) => {
  return {
    type: 'create_action_permission' as const,
    args,
  };
};

const useSaveActionPermission = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const savePermission = async (
    args: SaveActionPermissionArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const query = getCreateActionPermissionQuery({
      role: args.role,
      action: args.action,
    });

    return mutate(
      {
        query,
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Permission saved successfully!',
            type: 'success',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: `Failed to save permissions for role "${args.role}"`,
            error: err,
          });
          options?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    savePermission,
    ...rest,
  };
};

export default useSaveActionPermission;
