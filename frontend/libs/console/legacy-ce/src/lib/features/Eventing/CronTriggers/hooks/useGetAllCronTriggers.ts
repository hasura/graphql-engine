import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { CronTrigger } from '@hasura/shared/types';
import { HttpError } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { runMetadataQuery } from '@hasura/metadata/api';

interface GetCronTriggersResponse {
  cron_triggers: CronTrigger[];
}

// https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/scheduled-triggers/#metadata-get-cron-triggers
const body = {
  type: 'get_cron_triggers' as const,
  args: {},
};

export type Options<FinalResult> = {
  select?: (m: CronTrigger[]) => FinalResult;
  staleTime?: number;
  enabled?: boolean;
};

export const ALL_CRON_TRIGGERS_QUERY_KEY = 'ALL_CRON_TRIGGERS';

export const useGetAllCronTriggers = <FinalResult = CronTrigger[]>(
  options?: Options<FinalResult>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<CronTrigger[], HttpError, FinalResult>({
    queryKey: [ALL_CRON_TRIGGERS_QUERY_KEY],
    queryFn: async () => {
      return runMetadataQuery<GetCronTriggersResponse>({
        url: endpoints.metadata,
        fetchJson,
        body,
      }).then((result) => result.cron_triggers);
    },
    ...options,
  });

  return queryReturn;
};
