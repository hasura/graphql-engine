import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { SupportedDriver, EventRequestTransform } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { getErrorMessage, transformHeaderConfigs } from '@hasura/shared/utils';
import { defaultState, type LocalEventTriggerState } from '../types';
import type { RequestTransformState } from '../../../ConfigureTransformation/stateDefaults';
import { getRequestTransformObject } from '../../../ConfigureTransformation/utils';
import { validateETState } from '../utils';
import { getDriverPrefix } from '@hasura/metadata/helpers';

const errorTitle = 'Creating event trigger failed';

export type CreateEventTriggerArgs = {
  state: LocalEventTriggerState;
  transformState: RequestTransformState;
  sourceKind: SupportedDriver;
};

export const generateCreateEventTriggerQuery = (
  state: LocalEventTriggerState,
  sourceKind: SupportedDriver,
  replace = false,
  requestTransform?: EventRequestTransform,
) => {
  const prefix = getDriverPrefix(sourceKind);

  return {
    type: `${prefix}_create_event_trigger` as const,
    args: {
      name: state.name.trim(),
      table: state.table,
      source: state.source,
      webhook:
        state.webhook.type === 'static' ? state.webhook.value.trim() : null,
      webhook_from_env:
        state.webhook.type === 'env' ? state.webhook.value.trim() : null,
      insert: state.operations.includes('INSERT')
        ? {
            columns: '*',
          }
        : null,
      update: state.operations.includes('UPDATE')
        ? {
            columns: state.isAllColumnChecked ? '*' : state.operationColumns,
          }
        : null,
      delete: state.operations.includes('DELETE')
        ? {
            columns: '*',
          }
        : null,
      enable_manual: state.operations.includes('MANUAL'),
      retry_conf: state.retryConf,
      ...(state.cleanupConfig
        ? {
            cleanup_config: {
              ...defaultState.cleanupConfig,
              ...state.cleanupConfig,
            },
          }
        : {}),
      replace,
      headers: transformHeaderConfigs(state?.headers),
      request_transform: requestTransform,
    },
  };
};

const useCreateEventTrigger = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (
      { state, transformState, sourceKind }: CreateEventTriggerArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      const validationError = validateETState(state);
      if (validationError) {
        hasuraToast({
          type: 'error',
          title: errorTitle,
          message: validationError,
        });
        return;
      }

      const requestTransform =
        getRequestTransformObject(transformState) ?? undefined;
      const upQuery = generateCreateEventTriggerQuery(
        state,
        sourceKind,
        false,
        requestTransform,
      );

      mutation.mutate(
        {
          query: upQuery,
        },
        {
          onSuccess: () => {
            onSuccess?.();

            hasuraToast({
              title: 'Created event trigger successfully!',
              type: 'success',
            });
          },
          onError: (err) => {
            onError?.(err);
            hasuraToast({
              title: errorTitle,
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

export default useCreateEventTrigger;
