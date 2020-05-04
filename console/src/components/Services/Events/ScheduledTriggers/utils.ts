import { LocalScheduledTriggerState } from './Add/state';
import { isObject } from '../../../Common/utils/jsUtils';
import { makeBaseTable } from '../../../Common/utils/pgUtils';
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

export const parseServerScheduledTrigger = (
  trigger: ScheduledTrigger
): LocalScheduledTriggerState => {
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
    retryConf: {
      timeout_sec: trigger.retry_conf.timeout_seconds,
      interval_sec: trigger.retry_conf.retry_interval_seconds,
      num_retries: trigger.retry_conf.num_retries,
      tolerance_sec: trigger.retry_conf.tolerance_seconds,
    },
  };
};

export const stEventsTable = makeBaseTable(
  'hdb_scheduled_events',
  'hdb_catalog',
  [
    { column_name: 'id', data_type: 'uuid' },
    { column_name: 'name', data_type: 'text' },
    { column_name: 'scheduled_time', data_type: 'timestamptz' },
    { column_name: 'cancelled', data_type: 'boolean' },
    { column_name: 'delivered', data_type: 'boolean' },
    { column_name: 'error', data_type: 'boolean' },
    { column_name: 'tries', data_type: 'int' },
    { column_name: 'created_at', data_type: 'timestamptz' },
    { column_name: 'locked', data_type: 'text' },
    { column_name: 'next_retry_at', data_type: 'timestamptz' },
    { column_name: 'dead', data_type: 'boolean' },
  ]
);

export const stInvocationLogsTable = makeBaseTable(
  'event_invocation_logs',
  'hdb_catalog',
  [
    { column_name: 'id', data_type: 'uuid' },
    { column_name: 'event_id', data_type: 'uuid' },
    { column_name: 'status', data_type: 'int' },
    { column_name: 'request', data_type: 'text' },
    { column_name: 'response', data_type: 'text' },
    { column_name: 'created_at', data_type: 'timestamptz' },
  ]
);
