import { RequestTransform } from '../../../../../metadata/types';
import { Schema } from '../schema';
import {
  emptyDefaultValues,
  serverHeadersToKeyValueArray,
  stringifyNumberValue,
} from './utils';
import { useGetAllCronTriggers } from './useGetAllCronTriggers';

interface Props {
  cronTriggerName?: string;
}

export const useDefaultValues = (props: Props) => {
  const { cronTriggerName } = props;
  const { data, isLoading, isError } = useGetAllCronTriggers();

  if (!cronTriggerName) {
    // if cron trigger name is not passed to the component, then we are creating a new cron trigger
    // directly return empty default values
    return { data: emptyDefaultValues, isLoading: false, isError: false };
  }

  if (!isLoading && !isError) {
    // if cron trigger name is passed to the component, then we are updating an existing cron trigger
    // wait for fetching all cron triggers api call to resolve, then find the cron trigger
    const currentTrigger = data?.cron_triggers.find(
      cronTrigger => cronTriggerName === cronTrigger.name
    );

    if (!currentTrigger) {
      return { data: emptyDefaultValues, isLoading, isError: true };
    }

    const existingCronTriggerValues: Schema & {
      requestTransform?: RequestTransform;
    } = {
      name: currentTrigger.name,
      webhook: currentTrigger.webhook,
      schedule: currentTrigger.schedule,
      payload: JSON.stringify(currentTrigger.payload),
      headers: serverHeadersToKeyValueArray(currentTrigger.headers),
      num_retries: stringifyNumberValue(
        currentTrigger.retry_conf?.num_retries,
        '0'
      ),
      retry_interval_seconds: stringifyNumberValue(
        currentTrigger.retry_conf?.retry_interval_seconds,
        '10'
      ),
      timeout_seconds: stringifyNumberValue(
        currentTrigger.retry_conf?.timeout_seconds,
        '60'
      ),
      tolerance_seconds: stringifyNumberValue(
        currentTrigger.retry_conf?.tolerance_seconds,
        '21600'
      ),
      include_in_metadata: currentTrigger.include_in_metadata,
      comment: currentTrigger.comment ?? '',
    };
    return {
      data: existingCronTriggerValues,
      isLoading,
      isError,
      requestTransform: currentTrigger?.request_transform,
    };
  }

  // return error and loading states with empty default values
  return {
    data: emptyDefaultValues,
    isLoading,
    isError,
  };
};
