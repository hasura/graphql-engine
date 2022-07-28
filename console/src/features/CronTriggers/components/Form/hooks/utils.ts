import { Nullable } from '@/components/Common/utils/tsUtils';
import { ServerHeader } from '@/metadata/types';
import { Schema } from '../schema';

export const serverHeadersToKeyValueArray = (
  serverObject: ServerHeader[] | undefined
): Schema['headers'] => {
  if (serverObject) {
    return serverObject.map(header => {
      if ('value_from_env' in header) {
        return {
          name: header.name,
          value: header.value_from_env ?? '',
          type: 'from_env' as const,
        };
      }
      return {
        name: header.name,
        value: header.value ?? '',
        type: 'from_value' as const,
      };
    });
  }
  return [];
};

export const serverQueryParamsToKeyValueArray = (
  serverObject: Nullable<Record<string, string>>
): Schema['query_params'] => {
  if (serverObject) {
    return Object.entries(serverObject).map(([key, value]) => ({
      name: key,
      value,
    }));
  }
  return [];
};

export const stringifyNumberValue = (
  value: number | undefined,
  defaultValue: string
) => {
  if (value) {
    return String(value);
  }
  return defaultValue;
};

export const emptyDefaultValues: Schema = {
  name: '',
  webhook: '',
  schedule: '',
  payload: '',
  headers: [],
  num_retries: '0',
  retry_interval_seconds: '10',
  timeout_seconds: '60',
  include_in_metadata: true,
  comment: '',
  url_template: '',
  request_method: null,
  query_params: [],
};
