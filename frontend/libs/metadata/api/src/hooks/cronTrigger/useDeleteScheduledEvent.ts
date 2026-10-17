import type { CronTriggerType } from '@hasura/shared/types';
import { useQueryClient } from '@tanstack/react-query';
import { GET_SCHEDULED_EVENT_INVOCATIONS_QUERY_KEY } from './useGetScheduledEventInvocations';
import { GET_SCHEDULED_EVENTS_QUERY_KEY } from './useGetScheduledEvents';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from '../metadata';

export type DeleteScheduledEventArgs = {
  type: CronTriggerType;
  event_id: string;
};

export const useDeleteScheduledEvent = () => {
  const queryClient = useQueryClient();

  return useMetadataMigrationMutation(
    async (args: DeleteScheduledEventArgs) => ({
      type: 'delete_scheduled_event',
      args: args,
    }),
    {
      onSuccess() {
        queryClient.invalidateQueries({
          queryKey: [GET_SCHEDULED_EVENT_INVOCATIONS_QUERY_KEY],
        });

        queryClient.invalidateQueries({
          queryKey: [GET_SCHEDULED_EVENTS_QUERY_KEY],
        });

        hasuraToast({
          type: 'success',
          title: 'Successfully deleted event',
        });
      },
      onError(error) {
        showErrorNotification({
          title: 'Error in cancelling the event',
          error,
        });
      },
    },
  );
};
