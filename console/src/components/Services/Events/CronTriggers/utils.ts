import { LocalScheduledTriggerState } from './state';
import {
  isObject,
  isValidURL,
  isValidTemplateLiteral,
} from '../../../Common/utils/jsUtils';
import { Triggers, ScheduledTrigger } from '../types';
import { parseServerHeaders } from '../../../Common/Headers/utils';
import { BaseTable } from '../../../../dataSources/types';

export const validateAddState = (state: LocalScheduledTriggerState) => {
  if (!state.name) {
    return 'name cannot be empty';
  }
  if (!isValidURL(state.webhook) && !isValidTemplateLiteral(state.webhook)) {
    return 'webhook must either be a valid URL or a valid template literal';
  }

  if (!state.schedule) {
    return 'cron schedule is mandatory';
  }

  try {
    const maybeObj = JSON.parse(state.payload);
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
  return triggers.find(t => t.name === triggerName);
};

export const parseServerScheduledTrigger = (
  trigger: ScheduledTrigger
): LocalScheduledTriggerState => {
  return {
    name: trigger.name,
    webhook: trigger.webhook_conf,
    schedule: trigger.cron_schedule,
    payload: JSON.stringify(trigger.payload, null, 2),
    headers: parseServerHeaders(trigger.header_conf),
    loading: {
      modify: false,
      delete: false,
      add: false,
    },
    retryConf: {
      timeout_sec: trigger.retry_conf.timeout_seconds,
      interval_sec: trigger.retry_conf.retry_interval_seconds,
      num_retries: trigger.retry_conf.num_retries,
    },
    comment: trigger.comment,
    includeInMetadata: trigger.include_in_metadata,
  };
};

export const stEventsTable: BaseTable = {
  table_name: 'hdb_cron_events',
  table_schema: 'hdb_catalog',
  columns: [
    { column_name: 'id', data_type: 'uuid' },
    { column_name: 'name', data_type: 'text' },
    { column_name: 'scheduled_time', data_type: 'timestamptz' },
    { column_name: 'status', data_type: 'boolean' },
    { column_name: 'tries', data_type: 'int' },
    { column_name: 'created_at', data_type: 'timestamptz' },
    { column_name: 'next_retry_at', data_type: 'timestamptz' },
  ],
};

export const stInvocationLogsTable: BaseTable = {
  table_name: 'hdb_cron_event_invocation_logs',
  table_schema: 'hdb_catalog',
  columns: [
    { column_name: 'id', data_type: 'uuid' },
    { column_name: 'event_id', data_type: 'uuid' },
    { column_name: 'status', data_type: 'int' },
    { column_name: 'request', data_type: 'text' },
    { column_name: 'response', data_type: 'text' },
    { column_name: 'created_at', data_type: 'timestamptz' },
  ],
};
