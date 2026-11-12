import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata';

export type UpdateIntrospectionOptionsArgs = {
  existingOptions: string[];
  roleName: string;
  introspectionIsDisabled: boolean;
};

export const updateIntrospectionOptionsQuery = ({
  existingOptions,
  roleName,
  introspectionIsDisabled,
}: UpdateIntrospectionOptionsArgs) => {
  const updatedRoleList = [...existingOptions];
  if (introspectionIsDisabled && !existingOptions.includes(roleName)) {
    updatedRoleList.push(roleName);
  }

  if (!introspectionIsDisabled && existingOptions.includes(roleName)) {
    updatedRoleList.splice(updatedRoleList.indexOf(roleName), 1);
  }

  return {
    type: 'set_graphql_schema_introspection_options' as const,
    args: {
      disabled_for_roles: updatedRoleList,
    },
  };
};

export const useUpdateIntrospectionOptions = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (args: UpdateIntrospectionOptionsArgs, onSuccess?: () => void) => {
      mutation.mutate(
        {
          query: updateIntrospectionOptionsQuery(args),
        },
        {
          onSuccess: () => {
            onSuccess?.();

            hasuraToast({
              title: 'Updated Introspection!',
              type: 'success',
            });
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Updating Introspection options failed!',
              error: err,
            });
          },
        },
      );
    },
    [mutation],
  );
};

export default useUpdateIntrospectionOptions;
