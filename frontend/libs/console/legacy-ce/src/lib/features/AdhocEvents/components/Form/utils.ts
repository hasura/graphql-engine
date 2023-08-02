import { isJsonString } from '../../../../components/Common/utils/jsUtils';
import { ScheduledTrigger } from '../../../../metadata/types';
import { Schema } from './schema';

const transformFormData = (values: Schema) => {
  const apiPayload: ScheduledTrigger = {
    webhook: values.webhook,
    schedule_at: values.time,
    payload: isJsonString(values.payload)
      ? JSON.parse(values.payload)
      : values.payload,
    headers: values.headers.map(header => {
      if (header.type === 'from_env')
        return { name: header.name, value_from_env: header.value };
      return { name: header.name, value: header.value };
    }),
    retry_conf: {
      num_retries: Number(values.num_retries),
      retry_interval_seconds: Number(values.retry_interval_seconds),
      timeout_seconds: Number(values.timeout_seconds),
    },
    comment: values.comment,
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
