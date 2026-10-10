import { parseHeaderConfigs } from '@hasura/shared/utils';
import { Schema } from '../schema';
import { emptyDefaultValues, stringifyNumberValue } from './utils';
import { CronTrigger, RequestTransformBody } from '@hasura/shared/types';

interface Props {
  currentTrigger?: CronTrigger;
}

export const useDefaultValues = ({ currentTrigger }: Props) => {
  if (!currentTrigger) {
    // if cron trigger name is not passed to the component, then we are creating a new cron trigger
    // directly return empty default values
    return { data: emptyDefaultValues };
  }

  const existingCronTriggerValues: Schema & {
    requestTransform?: RequestTransformBody;
  } = {
    name: currentTrigger.name,
    webhook: currentTrigger.webhook,
    schedule: currentTrigger.schedule,
    payload: JSON.stringify(currentTrigger.payload),
    headers: parseHeaderConfigs(currentTrigger.headers),
    num_retries: stringifyNumberValue(
      currentTrigger.retry_conf?.num_retries,
      '0',
    ),
    retry_interval_seconds: stringifyNumberValue(
      currentTrigger.retry_conf?.retry_interval_seconds,
      '10',
    ),
    timeout_seconds: stringifyNumberValue(
      currentTrigger.retry_conf?.timeout_seconds,
      '60',
    ),
    tolerance_seconds: stringifyNumberValue(
      currentTrigger.retry_conf?.tolerance_seconds,
      '21600',
    ),
    include_in_metadata: currentTrigger.include_in_metadata,
    comment: currentTrigger.comment ?? '',
  };

  return {
    data: existingCronTriggerValues,
    requestTransform: currentTrigger?.request_transform,
  };
};
