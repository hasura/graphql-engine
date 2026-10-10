import { useAuthFetchJson } from '@hasura/shared/hooks';
import type { CronTriggerType } from '@hasura/shared/types';
import { useQueryClient } from '@tanstack/react-query';
import { GET_SCHEDULED_EVENT_INVOCATIONS_QUERY_KEY } from './useGetScheduledEventInvocations';
import { GET_SCHEDULED_EVENTS_QUERY_KEY } from './useGetScheduledEvents';
import { hasuraToast } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';
import { useAppContext } from '@hasura/shared/context';
import { runMetadataQuery } from '@hasura/metadata/api';

export type DeleteScheduledEventArgs = {
  type: CronTriggerType;
  event_id: string;
};

const useDeleteScheduledEvent = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return (args: DeleteScheduledEventArgs) => {
    return runMetadataQuery({
      url: endpoints.metadata,
      fetchJson,
      body: {
        type: 'delete_scheduled_event',
        args: args,
      },
    })
      .then(() => {
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
      })
      .catch((err) => {
        hasuraToast({
          type: 'error',
          title: 'Error in cancelling the event',
          message: getErrorMessage(err),
        });
      });
  };
};

export default useDeleteScheduledEvent;
