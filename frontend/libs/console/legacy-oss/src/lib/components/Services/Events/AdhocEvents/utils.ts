import { LocalAdhocEventState } from './Add/state';
import {
  isObject,
  isValidURL,
  isValidTemplateLiteral,
  isValidDate,
} from '../../../Common/utils/jsUtils';
import { BaseTable } from '../../../../dataSources/types';

export const validateAddState = (state: LocalAdhocEventState) => {
  if (!isValidURL(state.webhook) && !isValidTemplateLiteral(state.webhook)) {
    return 'webhook must either be a valid URL or a valid template literal';
  }

  if (!isValidDate(state.time)) {
    return 'the given time is invalid';
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

export const adhocEventsTable: BaseTable = {
  table_name: 'hdb_scheduled_events',
  table_schema: 'hdb_catalog',
  columns: [
    { column_name: 'id', data_type: 'uuid' },
    { column_name: 'webhook_conf', data_type: 'text' },
    { column_name: 'scheduled_time', data_type: 'timestamptz' },
    { column_name: 'retry_conf', data_type: 'jsonb' },
    { column_name: 'header_conf', data_type: 'jsonb' },
    { column_name: 'payload', data_type: 'string' },
    { column_name: 'status', data_type: 'text' },
    { column_name: 'tries', data_type: 'int' },
    { column_name: 'created_at', data_type: 'timestamptz' },
    { column_name: 'next_retry_at', data_type: 'timestamptz' },
    { column_name: 'comment', data_type: 'text' },
  ],
};

export const stInvocationLogsTable: BaseTable = {
  table_name: 'hdb_scheduled_event_invocation_logs',
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
