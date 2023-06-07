import { isJsonString } from '../../../../components/Common/utils/jsUtils';
import { CronTrigger, RequestTransform } from '../../../../metadata/types';
import { Schema } from './schema';

/**
 * Transforms form data to `create_cron_trigger` api payload
 * @param values React hook form schema containing all form fields
 * @returns api payload for `create_cron_trigger`
 */
const transformFormData = (
  values: Schema,
  replace: boolean,
  requestTransform?: RequestTransform
) => {
  const apiPayload: CronTrigger & { replace?: true } = {
    name: values.name,
    webhook: values.webhook,
    schedule: values.schedule,
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
      tolerance_seconds: Number(values.tolerance_seconds),
    },
    include_in_metadata: values.include_in_metadata,
    comment: values.comment,
    request_transform: requestTransform,
    ...(replace && { replace: true }),
  };

  return apiPayload;
};

export const getCronTriggerCreateQuery = (
  values: Schema,
  requestTransform?: RequestTransform,
  replace = false
) => {
  const args = transformFormData(values, replace, requestTransform);
  return {
    type: 'create_cron_trigger' as const,
    args,
  };
};

export const getCronTriggerDeleteQuery = (name: string) => ({
  type: 'delete_cron_trigger' as const,
  args: {
    name,
  },
});

export const getCronTriggerUpdateQuery = (
  name: string,
  values: Schema,
  requestTransform?: RequestTransform
) => ({
  type: 'bulk' as const,
  args:
    name === values.name
      ? [getCronTriggerCreateQuery(values, requestTransform, true)]
      : [
          getCronTriggerDeleteQuery(name),
          getCronTriggerCreateQuery(values, requestTransform),
        ],
});
