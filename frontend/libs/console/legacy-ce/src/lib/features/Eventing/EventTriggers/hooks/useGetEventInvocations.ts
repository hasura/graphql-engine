import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { EventInvocation, QualifiedDataSource } from '@hasura/shared/types';
import { HttpError } from '@hasura/shared/types';
import { runMetadataQuery } from '@hasura/metadata/api';
import { useAppContext } from '@hasura/shared/context';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export const GET_EVENT_TRIGGER_INVOCATIONS_QUERY_KEY = 'GET_EVENT_INVOCATIONS';

export type GetEventInvocationsArgs = {
  name: string;
  source: QualifiedDataSource;
  limit?: number;
  offset?: number;
};

export type Options<FinalResult> = {
  select?: (m: EventInvocation[]) => FinalResult;
  staleTime?: number;
  enabled?: boolean;
};

const useGetEventInvocations = <FinalResult = EventInvocation[]>(
  { source, ...args }: GetEventInvocationsArgs,
  options?: Options<FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<EventInvocation[], HttpError, FinalResult>({
    queryKey: [GET_EVENT_TRIGGER_INVOCATIONS_QUERY_KEY, source.name, args],
    queryFn: async () => {
      return runMetadataQuery({
        url: endpoints.metadata,
        fetchJson,
        body: {
          type: `${getDriverPrefix(source.kind)}_get_event_invocation_logs` as const,
          args: { ...args, source: source.name },
        },
      });
    },
    ...options,
  });

  return queryReturn;
};

export default useGetEventInvocations;
