import { useQuery } from '@tanstack/react-query';
import type { ScheduledEventInvocation } from '@hasura/shared/types';
import { HttpError } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { runMetadataQuery } from '@hasura/metadata/api';

export type GetScheduledEventInvocationsArgs = {
  limit?: number;
  offset?: number;
  get_rows_count?: boolean;
  event_id?: string;
} & (
  | {
      type: 'cron';
      trigger_name: string;
    }
  | {
      type: 'one_off';
    }
);

export const GET_SCHEDULED_EVENT_INVOCATIONS_QUERY_KEY =
  'GET_SCHEDULED_EVENT_INVOCATIONS';

export type Options<FinalResult> = {
  select?: (m: ScheduledEventInvocation[]) => FinalResult;
  staleTime?: number;
  enabled?: boolean;
};

const useGetScheduledEventInvocations = <
  FinalResult = ScheduledEventInvocation[],
>(
  args: GetScheduledEventInvocationsArgs,
  options?: Options<FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<
    ScheduledEventInvocation[],
    HttpError,
    FinalResult
  >({
    queryKey: [GET_SCHEDULED_EVENT_INVOCATIONS_QUERY_KEY, args],
    queryFn: async () => {
      return runMetadataQuery<{ invocations: ScheduledEventInvocation[] }>({
        url: endpoints.metadata,
        fetchJson,
        body: {
          type: 'get_scheduled_event_invocations',
          args: args,
        },
      }).then((result) => result.invocations);
    },
    ...options,
  });

  return queryReturn;
};

export default useGetScheduledEventInvocations;
