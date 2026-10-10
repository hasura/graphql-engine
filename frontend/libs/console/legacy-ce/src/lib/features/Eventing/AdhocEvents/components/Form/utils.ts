import { isJsonString, transformHeaderConfigs } from '@hasura/shared/utils';
import { Schema } from './schema';
import { ScheduledEventCreateArgs } from '@hasura/shared/types';

const transformFormData = (values: Schema) => {
  const apiPayload: ScheduledEventCreateArgs = {
    webhook: values.webhook,
    schedule_at: values.time,
    payload: isJsonString(values.payload)
      ? JSON.parse(values.payload)
      : values.payload,
    headers: transformHeaderConfigs(values.headers),
    retry_conf: {
      num_retries: Number(values.num_retries),
      retry_interval_seconds: Number(values.retry_interval_seconds),
      timeout_seconds: Number(values.timeout_seconds),
    },
    comment: values.comment,
    name: '',
    include_in_metadata: true,
  };

  return apiPayload;
};

export const getScheduledEventCreateQuery = (values: Schema) => {
  const args = transformFormData(values);
  return {
    type: 'create_scheduled_event' as const,
    args,
  };
};
