import { useQuery } from '@tanstack/react-query';
import { EventInvocation, QualifiedDataSource } from '@hasura/shared/types';
import { HttpError } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { useAppContext } from '@hasura/shared/context';
import { runMetadataQuery } from '@hasura/metadata/api';

export type GetEventByIdArgs = {
  source: QualifiedDataSource | undefined;
  event_id: string;
  invocation_log_limit?: number;
  invocation_log_offset?: number;
};

export const GET_EVENTS_BY_ID_QUERY_KEY = 'GET_EVENTS_BY_ID';

export type Options<FinalResult> = {
  select?: (m: EventInvocation[]) => FinalResult;
  staleTime?: number;
  enabled?: boolean;
  refetchInterval?: number | false;
};

const useGetEventsById = <FinalResult = EventInvocation[]>(
  { source, ...args }: GetEventByIdArgs,
  options?: Options<FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<EventInvocation[], HttpError, FinalResult>({
    queryKey: [GET_EVENTS_BY_ID_QUERY_KEY, source?.name, args],
    queryFn: async () => {
      if (!source) {
        return [];
      }

      const apiPrefix = getDriverPrefix(source.kind);

      return runMetadataQuery<{ invocations: EventInvocation[] }>({
        url: endpoints.metadata,
        fetchJson,
        body: {
          type: `${apiPrefix}_get_event_by_id`,
          args: {
            ...args,
            source: source.name,
          },
        },
      }).then((result) => result.invocations);
    },
    ...options,
    enabled: Boolean(source) || options?.enabled !== false,
  });

  return queryReturn;
};

export default useGetEventsById;
