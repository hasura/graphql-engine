import { LocalEventTriggerState } from './state';
import { isValidURL } from '../../../Common/utils/jsUtils';
import { makeBaseTable } from '../../../Common/utils/pgUtils';

// check 2xx success status codes

const REQUEST = 'request';
const RESPONSE = 'response';
const VERSION_TWO = '2';

export const parseRowData = (row: any, dataType: any) => {
  switch (dataType) {
    case REQUEST: {
      switch (row.request.version) {
        case VERSION_TWO: {
          const data = row.request.payload;
          return {
            data,
            headers: row.request.headers,
          };
        }
        default:
          return {
            data: row.request,
          };
      }
    }

    case RESPONSE: {
      let data;
      switch (row.response.version) {
        case VERSION_TWO: {
          try {
            // Handle graphql-engine server error message
            if (row.response.data.message) {
              data = row.response.data;
            } else {
              data = JSON.parse(row.response.data.body);
            }
          } catch (e) {
            console.error(e);
            data = row.response.data.body;
          }
          return {
            data,
            headers: row.response.data.headers,
            status_code: row.response.data.status,
          };
        }
        default: {
          try {
            data = JSON.parse(row.response);
          } catch (e) {
            console.error(e);
            data = row.response;
          }
          return {
            data,
            status_code: row.status,
          };
        }
      }
    }
    default:
      return false;
  }
};

export const validateAddETState = (state: LocalEventTriggerState) => {
  if (!state.name) {
    return 'Trigger name cannot be empty';
  }
  if (!state.table.name) {
    return 'Table cannot be empty';
  }
  if (!state.webhook.value) {
    return 'Webhook URL cannot be empty';
  }
  if (state.webhook.type === 'static' && !isValidURL(state.webhook.value)) {
    return 'Invalid webhook URL';
  }

  return null;
};

export const etEventsTable = makeBaseTable('event_log', 'hdb_catalog', [
  { column_name: 'id', data_type: 'uuid' },
  { column_name: 'trigger_name', data_type: 'text' },
  { column_name: 'payload', data_type: 'jsonb' },
  { column_name: 'delivered', data_type: 'text' },
  { column_name: 'error', data_type: 'boolean' },
  { column_name: 'tries', data_type: 'int' },
  { column_name: 'created_at', data_type: 'timestamptz' },
  { column_name: 'locked', data_type: 'boolean' },
  { column_name: 'next_retry_at', data_type: 'timestamptz' },
  { column_name: 'archived', data_type: 'boolean' },
]);

export const etInvocationLogsTable = makeBaseTable(
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
