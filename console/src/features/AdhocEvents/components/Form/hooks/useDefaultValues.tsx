import { Schema } from '../schema';

export const useDefaultValues = () => {
  const emptyDefaultValues: Schema = {
    webhook: '',
    time: new Date(),
    payload: '',
    headers: [],
    num_retries: '0',
    retry_interval_seconds: '10',
    timeout_seconds: '60',
    comment: '',
  };
  return { data: emptyDefaultValues };
};
