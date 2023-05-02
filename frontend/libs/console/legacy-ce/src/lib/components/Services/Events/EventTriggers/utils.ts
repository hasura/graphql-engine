import { LocalEventTriggerState } from './state';
import { BaseTable } from '../../../../dataSources/types';
import { isURLTemplated, isValidURL } from '../../../Common/utils/jsUtils';
import { ETOperationColumn, EventTriggerOperation } from '../types';

// check 2xx success status codes

const REQUEST = 'request' as const;
const RESPONSE = 'response' as const;
const VERSION_TWO = '2';

type DataType = typeof REQUEST | typeof RESPONSE;

export const parseRowData = (row: any, dataType: DataType) => {
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

export const validateETState = (state: LocalEventTriggerState) => {
  if (!state.name) {
    return 'Trigger name cannot be empty';
  }
  if (!state.table.name) {
    return 'Table cannot be empty';
  }
  if (!state.webhook.value) {
    return 'Webhook URL cannot be empty';
  }
  if (
    state.webhook.type === 'static' &&
    !(isValidURL(state.webhook.value) || isURLTemplated(state.webhook.value))
  ) {
    return 'Invalid webhook URL';
  }

  return null;
};

export const etEventsTable: BaseTable = {
  table_name: 'event_log',
  table_schema: 'hdb_catalog',
  columns: [
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
  ],
};

export const etInvocationLogsTable: BaseTable = {
  table_name: 'event_invocation_logs',
  table_schema: 'hdb_catalog',
  columns: [
    { column_name: 'id', data_type: 'uuid' },
    { column_name: 'event_id', data_type: 'uuid' },
    { column_name: 'http_status', data_type: 'int' },
    { column_name: 'request', data_type: 'text' },
    { column_name: 'response', data_type: 'text' },
    { column_name: 'created_at', data_type: 'timestamptz' },
  ],
};

type ColVals = string | number | boolean;

/**
 * Returns an example value to be used in `Sample Input` section of `Rest connectors`
 * We don't need strict checking of all types as its used for sample input
 * @param type Type of the table column, varies with different databases
 * @param value Name of the table column
 */
const getValueFromDataType = (type: string, value: string): ColVals => {
  const maxNum = 20;
  const typeStr = type.toLowerCase();
  if (typeStr === 'integer' || typeStr === 'numeric' || typeStr === 'int') {
    return Math.floor(Math.random() * maxNum);
  } else if (typeStr === 'boolean') {
    return true;
  } else if (typeStr.includes('date') || typeStr.includes('timestamp')) {
    return new Date().toISOString();
  }
  return value;
};

const getColumnDataFromOpCols = (
  operationColumns: ETOperationColumn[],
  opValue: string,
  isAllColumnsChecked?: boolean
): Record<string, ColVals> => {
  const colData: Record<string, ColVals> = {};
  operationColumns.forEach(op => {
    if (
      opValue === 'INSERT' ||
      opValue === 'MANUAL' ||
      opValue === 'DELETE' ||
      isAllColumnsChecked
    ) {
      colData[op.name] = getValueFromDataType(op.type, op.name);
    } else if (op.enabled) {
      colData[op.name] = getValueFromDataType(op.type, op.name);
    }
  });
  return colData;
};

const getDataFromOpCols = (
  opValue: string,
  operationColumns: ETOperationColumn[],
  isAllColumnsChecked?: boolean
) => {
  const opData: {
    old: Record<string, ColVals> | null;
    new: Record<string, ColVals> | null;
  } = { old: null, new: null };
  if (opValue === 'INSERT' || opValue === 'UPDATE' || opValue === 'MANUAL') {
    opData.new = getColumnDataFromOpCols(
      operationColumns,
      opValue,
      isAllColumnsChecked
    );
  }
  if (opValue === 'UPDATE' || opValue === 'DELETE') {
    opData.old = getColumnDataFromOpCols(
      operationColumns,
      opValue,
      isAllColumnsChecked
    );
  }
  return opData;
};

const getOperationValue = (op: Record<EventTriggerOperation, boolean>) => {
  if (op.update) {
    return 'UPDATE';
  } else if (op.insert) {
    return 'INSERT';
  } else if (op.delete) {
    return 'DELETE';
  } else if (op.enable_manual) {
    return 'MANUAL';
  }
  return '';
};

export const getEventRequestSampleInput = (
  name?: string,
  tableName?: string,
  schemaName?: string,
  retries?: number,
  operationColumns?: ETOperationColumn[],
  operations?: Record<EventTriggerOperation, boolean>,
  isAllColumnsChecked?: boolean
) => {
  const opValue = operations ? getOperationValue(operations) : '';
  const opData =
    opValue && operationColumns
      ? getDataFromOpCols(opValue, operationColumns, isAllColumnsChecked)
      : { old: null, new: null };

  const obj = {
    event: {
      op: opValue,
      data: opData,
      trace_context: {
        trace_id: '501ad47ed3570385',
        span_id: 'd586cc98cee55ad1',
      },
    },
    created_at: new Date().toISOString(),
    id: '2c173942-a860-4a4c-ab71-9a29e2384d54',
    delivery_info: { max_retries: retries ?? 0, current_retry: 0 },
    trigger: { name: name ?? 'triggerName' },
    table: {
      schema: schemaName ?? 'schemaName',
      name: tableName ?? 'tableName',
    },
  };

  const value = JSON.stringify(obj, null, 2);
  return value;
};

export const getCronTriggerRequestSampleInput = () => {
  const obj = {
    comment: 'comment',
    id: '06af0430-e4d8-4335-8659-c27225e8edfd',
    name: 'name',
    payload: {},
    scheduled_time: new Date().toISOString(),
  };

  const value = JSON.stringify(obj, null, 2);
  return value;
};
