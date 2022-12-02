import { useQuery } from 'react-query';
import Endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { CronTrigger } from '@/metadata/types';
import { useAppSelector } from '@/store';

interface GetCronTriggersResponse {
  cron_triggers: CronTrigger[];
}

export const useGetAllCronTriggers = () => {
  const headers = useAppSelector(state => state.tables.dataHeaders);

  // https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/scheduled-triggers/#metadata-get-cron-triggers
  const body = {
    type: 'get_cron_triggers',
    args: {},
  };

  return useQuery(['allCronTriggers'], () =>
    Api.post<GetCronTriggersResponse>({
      headers,
      body,
      url: Endpoints.metadata,
    })
  );
};
