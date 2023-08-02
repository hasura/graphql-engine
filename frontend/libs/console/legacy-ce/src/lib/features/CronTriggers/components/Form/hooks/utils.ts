import { ServerHeader } from '../../../../../metadata/types';
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
  tolerance_seconds: '21600',
  include_in_metadata: true,
  comment: '',
};
