import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import type { CronTriggerStatus, ScheduledEvent } from '@hasura/shared/types';
import { HttpError, OrderBy } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { runMetadataQuery } from '@hasura/metadata/api';

export type GetScheduledEventsArgs = {
  limit?: number;
  offset?: number;
  get_rows_count: boolean;
  status: CronTriggerStatus[];
  order_by: OrderBy[];
} & (
  | {
      type: 'cron';
      trigger_name: string;
    }
  | {
      type: 'one_off';
    }
);

export const GET_SCHEDULED_EVENTS_QUERY_KEY = 'GET_SCHEDULED_EVENTS';

export type Options<FinalResult> = {
  select?: (m: ScheduledEvent[]) => FinalResult;
  staleTime?: number;
  enabled?: boolean;
};

const useGetScheduledEvents = <FinalResult = ScheduledEvent[]>(
  args: GetScheduledEventsArgs,
  options?: Options<FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<ScheduledEvent[], HttpError, FinalResult>({
    queryKey: [GET_SCHEDULED_EVENTS_QUERY_KEY, args],
    queryFn: async () => {
      return runMetadataQuery<{ events: ScheduledEvent[] }>({
        url: endpoints.metadata,
        fetchJson,
        body: {
          type: 'get_scheduled_events',
          args: args,
        },
      }).then((result) => result.events);
    },
    ...options,
  });

  return queryReturn;
};

export default useGetScheduledEvents;
