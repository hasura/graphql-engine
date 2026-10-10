import {
  requestBodyActionState,
  requestTransformState,
  getEnvVarsFromLS,
  getSessionVarsFromLS,
  RequestTransformState,
} from '../../../../ConfigureTransformation';
import { isJsonString, transformHeaderConfigs } from '@hasura/shared/utils';
import { CronTrigger } from '@hasura/shared/types';
import type { EventRequestTransform } from '@hasura/shared/types';
import { Schema } from './schema';

/**
 * Transforms form data to `create_cron_trigger` api payload
 * @param values React hook form schema containing all form fields
 * @returns api payload for `create_cron_trigger`
 */
const transformFormData = (
  values: Schema,
  replace: boolean,
  requestTransform?: EventRequestTransform,
) => {
  const apiPayload: CronTrigger & { replace?: true } = {
    name: values.name,
    webhook: values.webhook,
    schedule: values.schedule,
    payload: isJsonString(values.payload)
      ? JSON.parse(values.payload)
      : values.payload,
    headers: transformHeaderConfigs(values.headers),
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
  requestTransform?: EventRequestTransform,
  replace = false,
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
  requestTransform?: EventRequestTransform,
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

const getCronTriggerRequestSampleInput = () => {
  const obj = {
    comment: 'comment',
    id: '06af0430-e4d8-4335-8659-c27225e8edfd',
    name: 'name',
    payload: {},
    scheduled_time: new Date().toISOString(),
  };

  const value = JSON.stringify(obj, null, 2);
  return value;
};

export const getCronTriggerRequestTransformDefaultState =
  (): RequestTransformState => {
    return {
      ...requestTransformState,
      envVars: getEnvVarsFromLS(),
      sessionVars: getSessionVarsFromLS(),
      requestQueryParams: [{ name: '', value: '' }],
      requestAddHeaders: [{ name: '', value: '' }],
      requestBody: {
        action: requestBodyActionState.transformApplicationJson,
        template: `{
  "payload": {{$body.payload}}
}`,
        form_template: [{ name: 'payload', value: '{{$body.payload}}' }],
      },
      requestSampleInput: getCronTriggerRequestSampleInput(),
    };
  };
