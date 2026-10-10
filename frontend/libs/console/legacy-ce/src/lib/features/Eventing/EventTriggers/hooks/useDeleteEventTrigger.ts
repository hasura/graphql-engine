import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { SupportedDriver } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { getErrorMessage, getConfirmation } from '@hasura/shared/utils';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type DeleteEventTriggerArgs = {
  name: string;
  source: { name: string; kind: SupportedDriver };
};

const useDeleteEventTrigger = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (args: DeleteEventTriggerArgs, onSuccess?: () => void) => {
      const isOk = getConfirmation(
        `This will permanently delete the event trigger and the associated metadata`,
        true,
        args.name,
      );
      if (!isOk) {
        return undefined;
      }

      const prefix = getDriverPrefix(args.source.kind);

      mutation.mutate(
        {
          query: {
            type: `${prefix}_delete_event_trigger`,
            args: {
              source: args.source.name,
              name: args.name.trim(),
            },
          },
        },
        {
          onSuccess: (data) => {
            onSuccess?.();

            hasuraToast({
              title: 'Deleted event trigger!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: 'Deleting event trigger failed!',
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

export default useDeleteEventTrigger;
