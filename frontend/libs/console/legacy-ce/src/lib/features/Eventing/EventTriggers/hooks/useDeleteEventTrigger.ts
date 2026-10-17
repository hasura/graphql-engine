import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { SupportedDriver } from '@hasura/shared/types';
import { hasuraToast, useDestructivePrompt } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type DeleteEventTriggerArgs = {
  name: string;
  source: { name: string; kind: SupportedDriver };
};

const useDeleteEventTrigger = () => {
  const mutation = useMetadataMigration();
  const destructivePrompt = useDestructivePrompt();

  return useCallback(
    async (args: DeleteEventTriggerArgs, onSuccess?: () => void) => {
      const prefix = getDriverPrefix(args.source.kind);

      destructivePrompt({
        resourceName: args.name,
        resourceType: 'event trigger',
        destroyTerm: 'delete',
        appendTerm: 'This also deletes the associated metadata.',
        onConfirm: () =>
          new Promise<boolean>((resolve) => {
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
                onSuccess: () => {
                  onSuccess?.();

                  hasuraToast({
                    title: 'Deleted event trigger!',
                    type: 'success',
                  });
                  resolve(true);
                },
                onError: (err) => {
                  hasuraToast({
                    title: 'Deleting event trigger failed!',
                    message: getErrorMessage(err),
                    type: 'error',
                  });
                  resolve(false);
                },
              },
            );
          }),
      });
    },
    [mutation, destructivePrompt],
  );
};

export default useDeleteEventTrigger;
