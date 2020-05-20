import { LocalAdhocEventState } from './Add/state';
import {
  isObject,
  isValidURL,
  isValidTemplateLiteral,
  isValidDate,
} from '../../../Common/utils/jsUtils';
import { makeBaseTable } from '../../../Common/utils/pgUtils';
import { getCreateScheduledEventQuery } from '../../../Common/utils/v1QueryUtils';

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

export const adhocEventsTable = makeBaseTable(
  'hdb_scheduled_events',
  'hdb_catalog',
  [
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
  ]
);

export const stInvocationLogsTable = makeBaseTable(
  'hdb_scheduled_event_invocation_logs',
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

export const SAMPLE_SCHEDDULE_EVENT_QUERY = JSON.stringify(
  getCreateScheduledEventQuery({
    webhook: 'http://httpbin.org/post',
    time: new Date(),
    payload: `{
    "key": "value",
    "foo": "bar"
  }`,
    headers: [
      {
        name: 'key1',
        type: 'static',
        value: 'value',
      },
      {
        name: 'key2',
        type: 'env',
        value: 'HASURA_GRAPHQL_ENV_VAR',
      },
    ],
    retryConf: {
      num_retries: 0,
      interval_sec: 60,
      timeout_sec: 60,
      tolerance_sec: 21600,
    },
    comment: 'This is a sample event',
    loading: false,
  }),
  null,
  4
);

export const SCHEDULED_EVENT_PAYLOAD_EDITOR_MAXLINES =
  Array(SAMPLE_SCHEDDULE_EVENT_QUERY.length)
    .fill(null)
    .reduce((count, _, i) => {
      if (SAMPLE_SCHEDDULE_EVENT_QUERY.charAt(i) === '\n') {
        return count + 1;
      }
      return count;
    }, 0) + 1;
