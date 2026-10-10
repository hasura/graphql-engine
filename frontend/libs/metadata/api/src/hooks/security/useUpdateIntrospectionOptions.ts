import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { getErrorMessage } from '@hasura/shared/utils';
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
            hasuraToast({
              title: 'Updating Introspection options failed!',
              message: getErrorMessage(err),
              type: 'error',
            });
          },
        },
      );
    },
    [mutation],
  );
};

export default useUpdateIntrospectionOptions;
