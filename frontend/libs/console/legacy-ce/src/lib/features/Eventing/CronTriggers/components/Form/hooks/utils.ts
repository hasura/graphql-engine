import { Schema } from '../schema';

export const stringifyNumberValue = (
  value: number | undefined,
  defaultValue: string,
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
