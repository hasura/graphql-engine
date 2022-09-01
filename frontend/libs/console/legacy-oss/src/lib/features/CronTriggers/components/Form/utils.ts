import { isEmpty, isJsonString } from '@/components/Common/utils/jsUtils';
import { CronTrigger, RequestTransform } from '@/metadata/types';
import { Schema } from './schema';

type RequestTransformBlockProps = {
  url_template: Schema['url_template'];
  request_method: Schema['request_method'];
  query_params: Schema['query_params'];
};

/**
 * Get request transform / rest connectors field of the cron trigger object, this field is added optionally
 * if at least one of the transform fields are present
 */
const getRequestTransformBlock = (
  props: RequestTransformBlockProps
): RequestTransform | undefined => {
  const { url_template, request_method, query_params } = props;

  // add transform block if atleast one of the field is present
  if (isEmpty(url_template) && isEmpty(query_params) && isEmpty(request_method))
    return;

  return {
    version: 2,
    template_engine: 'Kriti',
    method: request_method,
    url: url_template,
    query_params: query_params.reduce(
      (allParams, param) => ({
        ...allParams,
        [param.name]: param.value,
      }),
      {}
    ),
    // in case of GET requests, do not send request body or content-type header to the webhook
    // https://github.com/hasura/graphql-engine/issues/7937
    ...(request_method === 'GET' && {
      request_headers: {
        remove_headers: ['content-type'],
      },
      body: {
        action: 'remove',
      },
    }),
  };
};

/**
 * Transforms form data to `create_cron_trigger` api payload
 * @param values React hook form schema containing all form fields
 * @returns api payload for `create_cron_trigger`
 */
const transformFormData = (values: Schema) => {
  const { url_template, request_method, query_params } = values;

  const apiPayload: CronTrigger = {
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
    },
    include_in_metadata: values.include_in_metadata,
    comment: values.comment,
    request_transform: getRequestTransformBlock({
      url_template,
      request_method,
      query_params,
    }),
  };

  return apiPayload;
};

export const getCronTriggerCreateQuery = (values: Schema) => {
  const args = transformFormData(values);
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

export const getCronTriggerUpdateQuery = (values: Schema) => ({
  type: 'bulk' as const,
  args: [
    getCronTriggerDeleteQuery(values.name),
    getCronTriggerCreateQuery(values),
  ],
});
