import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { HttpError, QualifiedDataSource } from '@hasura/shared/types';
import { EventLog } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export const GET_EVENT_LOGS_QUERY_KEY = 'GET_EVENT_LOGS';

// https://hasura.io/docs/2.0/api-reference/metadata-api/event-triggers/#metadata-pg-get-event-logs-syntax
export type GetEventLogsArgs = {
  name: string;
  source: QualifiedDataSource;
  // Type of event logs to be fetched. If status is not provided then all types of status are included.
  status: 'pending' | 'processed';
  limit?: number;
  offset?: number;
};

export type Options<FinalResult> = {
  select?: (m: EventLog[]) => FinalResult;
  staleTime?: number;
  enabled?: boolean;
};

export const getScheduledEventTrigger = ({
  source,
  ...args
}: GetEventLogsArgs) => {
  return {
    type: `${getDriverPrefix(source.kind)}_get_event_logs` as const,
    args: {
      ...args,
      source: source.name,
    },
  };
};

const useGetEventLogs = <FinalResult = EventLog[]>(
  args: GetEventLogsArgs,
  options?: Options<FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<EventLog[], HttpError, FinalResult>({
    queryKey: [GET_EVENT_LOGS_QUERY_KEY, args],
    queryFn: async () => {
      const queryArgs = getScheduledEventTrigger(args);
      const fetchOptions = {
        method: 'POST',
        body: JSON.stringify(queryArgs),
      };

      return fetchJson(endpoints.metadata, fetchOptions);
    },
    ...options,
  });

  return queryReturn;
};

export default useGetEventLogs;
