import Endpoints from '../../../../../Endpoints';
import { Api } from '../../../../../hooks/apiUtils';
import { useAppSelector } from '../../../../../storeHooks';
import { useQuery, UseQueryOptions } from 'react-query';
import { CronTriggerAPIResult, ScheduledTrigger } from '../../types';

export const CRON_TRIGGERS_QUERY_KEY = 'cronTrigger';

export function useGetCronTriggers(
  queryOptions?: Omit<
    UseQueryOptions<
      { cron_triggers: CronTriggerAPIResult[] },
      Error,
      ScheduledTrigger[],
      'cronTrigger'
    >,
    'queryKey' | 'queryFn' | 'select'
  >
) {
  const body = {
    type: 'get_cron_triggers',
    args: {},
  };

  const headers = useAppSelector(state => state.tables.dataHeaders);
  const queryFn = () => {
    return Api.post<{ cron_triggers: CronTriggerAPIResult[] }>({
      headers,
      body,
      url: Endpoints.metadata,
    });
  };

  return useQuery({
    queryKey: CRON_TRIGGERS_QUERY_KEY,
    queryFn,
    ...queryOptions,
    select: data => {
      return data.cron_triggers.map(cron => ({
        name: cron.name,
        payload: cron.payload,
        retry_conf: {
          num_retries: cron.retry_conf?.num_retries,
          retry_interval_seconds: cron.retry_conf?.retry_interval_seconds,
          timeout_seconds: cron.retry_conf?.timeout_seconds,
          tolerance_seconds: cron.retry_conf?.tolerance_seconds,
        },
        header_conf: cron.headers,
        webhook_conf: cron.webhook,
        cron_schedule: cron.schedule,
        include_in_metadata: cron.include_in_metadata,
        comment: cron.comment,
      }));
    },
  });
}
