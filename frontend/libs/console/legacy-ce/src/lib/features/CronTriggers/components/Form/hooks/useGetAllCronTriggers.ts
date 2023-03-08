import { useQuery } from 'react-query';
import Endpoints from '../../../../../Endpoints';
import { Api } from '../../../../../hooks/apiUtils';
import { CronTrigger } from '../../../../../metadata/types';
import { useAppSelector } from '../../../../../storeHooks';

interface GetCronTriggersResponse {
  cron_triggers: CronTrigger[];
}

export const ALL_CRON_TRIGGERS_QUERY_KEY = 'allCronTriggers';
export const useGetAllCronTriggers = () => {
  const headers = useAppSelector(state => state.tables.dataHeaders);

  // https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/scheduled-triggers/#metadata-get-cron-triggers
  const body = {
    type: 'get_cron_triggers',
    args: {},
  };

  return useQuery([ALL_CRON_TRIGGERS_QUERY_KEY], () =>
    Api.post<GetCronTriggersResponse>({
      headers,
      body,
      url: Endpoints.metadata,
    })
  );
};
