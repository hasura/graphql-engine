import { hasuraToast } from '@hasura/shared/ui';
import { useCallback } from 'react';
import { getErrorMessage, isEmpty } from '@hasura/shared/utils';
import type { ApiLimits, RateLimit } from '@hasura/shared/types';
import { useMetadataMigration } from '../metadata';

export const apiLimitsFieldNames = [
  'depth_limit',
  'node_limit',
  'rate_limit',
  'time_limit',
  'batch_limit',
] as const;

type APILimitInputType<T> = {
  global: T;
  per_role?: Record<string, T>;
  state: 'disabled' | 'enabled' | 'global';
};

export type ApiLimitInput = {
  existingAPILimits?: ApiLimits;
  newAPILimits: {
    disabled: boolean;
    depth_limit?: APILimitInputType<number>;
    batch_limit?: APILimitInputType<number>;
    node_limit?: APILimitInputType<number>;
    time_limit?: APILimitInputType<number>;
    rate_limit?: RateLimit;
  };
};

export const updateAPILimitsQuery = ({
  existingAPILimits,
  newAPILimits,
}: ApiLimitInput) => {
  const reqBody: ApiLimits = {
    ...existingAPILimits,
    disabled: newAPILimits.disabled,
  };

  apiLimitsFieldNames.forEach((key) => {
    const role =
      newAPILimits[key]?.per_role && !isEmpty(newAPILimits[key]?.per_role)
        ? Object.keys(newAPILimits[key]?.per_role ?? {})[0]
        : 'global';
    switch (
      `${newAPILimits[key]?.state ?? 'default'}-${
        role === 'global' ? 'global' : 'per_role'
      }`
    ) {
      case 'disabled-global': {
        delete reqBody[key];
        break;
      }
      case 'enabled-global': {
        Object.assign(reqBody, {
          [key]: {
            ...existingAPILimits?.[key],
            global: newAPILimits[key]?.global,
          },
        });
        break;
      }
      case 'disabled-per_role': {
        if (reqBody[key]?.per_role) {
          delete reqBody[key]?.per_role?.[role];
        }
        break;
      }
      case 'enabled-per_role': {
        if (
          newAPILimits[key]?.per_role &&
          newAPILimits?.[key]?.per_role?.[role]
        ) {
          Object.assign(reqBody, {
            [key]: {
              global: newAPILimits[key]?.global,
              per_role: {
                ...existingAPILimits?.[key]?.per_role,
                [role]: newAPILimits?.[key]?.per_role?.[role],
              },
            },
          });
        }
        break;
      }
      case 'global-per_role': {
        if (reqBody[key]?.per_role) {
          delete reqBody[key]?.per_role?.[role];
        }
        break;
      }
      default:
    }
  });

  return {
    type: 'set_api_limits' as const,
    args: reqBody,
  };
};

export const useUpdateAPILimits = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (input: ApiLimitInput, onSuccess?: () => void) => {
      mutation.mutate(
        {
          query: updateAPILimitsQuery(input),
        },
        {
          onSuccess: (data) => {
            onSuccess?.();

            if (
              !Array.isArray(data) ||
              !data?.length ||
              typeof data[0].warnings !== 'object'
            ) {
              hasuraToast({
                title: 'Updated API limits!',
                type: 'success',
              });

              return;
            }

            const warnings = data
              .map((item) => item.warnings?.message)
              .filter(Boolean)
              .join('. ');

            hasuraToast({
              type: 'warning',
              title: 'Time Limit Exceeded System Limit',
              message: warnings,
              toastOptions: {
                duration: Infinity,
              },
            });
          },
          onError: (err) => {
            hasuraToast({
              title: 'Updating API limits failed!',
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

export default useUpdateAPILimits;
