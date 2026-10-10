import { useCallback } from 'react';
import { getRequestTransformObject } from '../../../ConfigureTransformation/utils';
import { hasuraToast } from '@hasura/shared/ui';
import {
  getErrorMessage,
  isURLTemplated,
  isValidURL,
  transformHeaderConfigs,
} from '@hasura/shared/utils';
import type { RequestTransformState } from '../../../ConfigureTransformation/stateDefaults';
import { useMetadataMigration } from '@hasura/metadata/api';
import { LocalEventTriggerState } from '../types';
import { parseServerETDefinition } from '../utils';
import { generateCreateEventTriggerQuery } from './useCreateEventTrigger';
import { TableColumn } from '@hasura/metadata/data-source';
import { EventTrigger, QualifiedDataSource } from '@hasura/shared/types';

const errorTitle = 'Saving event trigger failed';

export type EventTriggerProperty = 'webhook' | 'ops' | 'retry_conf' | 'headers';

export type ModifyEventTriggerArgs = {
  source: QualifiedDataSource;
  state: LocalEventTriggerState;
  transformState: RequestTransformState;
  trigger: EventTrigger;
  columns: TableColumn[];
  property?: EventTriggerProperty;
};

const useModifyEventTrigger = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (
      {
        source,
        state,
        transformState,
        columns,
        trigger,
        property,
      }: ModifyEventTriggerArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      const requestTransform =
        getRequestTransformObject(transformState) ?? undefined;

      const upQuery = generateCreateEventTriggerQuery(
        parseServerETDefinition({
          eventTrigger: trigger,
          columns,
          source: source.name,
          table: state.table!,
        }),
        source.kind,
        true,
        requestTransform,
      );

      switch (property) {
        case 'webhook': {
          if (
            state.webhook.type === 'static' &&
            !(
              isValidURL(state.webhook.value) ||
              isURLTemplated(state.webhook.value)
            )
          ) {
            return hasuraToast({
              type: 'error',
              title: errorTitle,
              message: 'Invalid URL',
            });
          }

          upQuery.args = {
            ...upQuery.args,
            webhook:
              state.webhook.type === 'static'
                ? state.webhook.value.trim()
                : null,
            webhook_from_env:
              state.webhook.type === 'env' ? state.webhook.value.trim() : null,
          };
          break;
        }
        case 'ops': {
          upQuery.args = {
            ...upQuery.args,
            insert: state.operations.includes('INSERT')
              ? { columns: '*' }
              : null,
            update: state.operations.includes('UPDATE')
              ? {
                  columns: state.isAllColumnChecked
                    ? '*'
                    : state.operationColumns,
                }
              : null,
            delete: state.operations.includes('DELETE')
              ? { columns: '*' }
              : null,
            enable_manual: state.operations.includes('MANUAL'),
          };
          break;
        }
        case 'retry_conf': {
          upQuery.args.retry_conf = state.retryConf;
          break;
        }
        case 'headers': {
          upQuery.args.headers = transformHeaderConfigs(state.headers);
          break;
        }
        default: {
          upQuery.args.cleanup_config = state.cleanupConfig;
          break;
        }
      }

      mutation.mutate(
        {
          query: upQuery,
        },
        {
          onSuccess: (data) => {
            onSuccess?.();

            hasuraToast({
              title: 'Saved event trigger successfully!',
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

export default useModifyEventTrigger;
