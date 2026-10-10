import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { getErrorMessage } from '@hasura/shared/utils';
import type { ApiLimits } from '@hasura/shared/types';
import { apiLimitsFieldNames } from './useUpdateAPILimits';
import { useMetadataMigration } from '../metadata';

export type RemoveAPILimitsArgs = {
  existingAPILimits?: ApiLimits;
  role: string;
};

export const removeAPILimitsQuery = ({
  existingAPILimits,
  role,
}: RemoveAPILimitsArgs) => {
  if (role === 'global') {
    return {
      type: 'remove_api_limits' as const,
      args: existingAPILimits ?? {},
    };
  }

  const args = apiLimitsFieldNames.reduce(
    (acc, key) => {
      if (!existingAPILimits?.[key]?.per_role?.[role]) {
        return acc;
      }

      acc[key] = {
        ...existingAPILimits[key],
        per_role: {
          ...existingAPILimits[key].per_role,
        },
      } as any;

      delete acc[key]?.per_role?.[role];

      return acc;
    },
    { ...existingAPILimits },
  );

  return {
    type: 'set_api_limits' as const,
    args,
  };
};

export const useRemoveAPILimits = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (args: RemoveAPILimitsArgs, onSuccess?: () => void) => {
      mutation.mutate(
        {
          query: removeAPILimitsQuery(args),
        },
        {
          onSuccess: (data) => {
            onSuccess?.();

            hasuraToast({
              title: 'Removed API limits!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: 'Removing API limits failed!',
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

export default useRemoveAPILimits;
