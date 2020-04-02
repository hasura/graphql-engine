import { LocalScheduledTriggerState } from './Add/state';
import { isObject } from '../../../Common/utils/jsUtils';
import { Triggers, ScheduledTrigger } from '../Types';
import { parseServerHeaders } from '../../../Common/Headers/utils';

export const validateAddState = (state: LocalScheduledTriggerState) => {
  if (!state.name) {
    return 'name cannot be empty';
  }
  if (!state.webhook) {
    return 'webhook cannot be empty';
  }
  if (state.webhook.type === 'static') {
    try {
      new URL(state.webhook.value);
    } catch (_) {
      return 'webhook must be a valid URL';
    }
  }
  if (state.schedule.type === 'cron') {
    if (!state.schedule.value) {
      return 'cron schedule cannot be empty';
    }
  }

  try {
    const maybeObj: any = JSON.parse(state.payload);
    if (!isObject(maybeObj)) {
      throw new Error();
    }
  } catch (_) {
    return 'payload must be valid JSON';
  }

  return '';
};

export const findScheduledTrigger = (
  triggers: Triggers,
  triggerName: string
) => {
  return triggers.scheduled.find(t => t.name === triggerName);
};

export const parseServerScheduledTrigger = (trigger: ScheduledTrigger) => {
  return {
    name: trigger.name,
    webhook: {
      type:
        typeof trigger.webhook_conf === 'string'
          ? 'static'
          : ('env' as 'static' | 'env'),
      value:
        typeof trigger.webhook_conf === 'string'
          ? trigger.webhook_conf
          : trigger.webhook_conf.from_env,
    },
    schedule: trigger.schedule_conf,
    payload: JSON.stringify(trigger.payload),
    headers: parseServerHeaders(trigger.header_conf),
    loading: false,
  };
};
