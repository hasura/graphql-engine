import {
  FILTER_TYPE_DROPDOWN,
  FILTER_TYPE_DROPDOWN_DEFAULT,
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
  ROLE_SYMBOL,
  CLIENT_NAME_SYMBOL,
  TIME_RANGE_SYMBOL,
  EMPTY_ROLE_SYMBOL,
  EMPTY_CLIENT_NAME_SYMBOL,
  ROLE_DISPLAY,
  TIME_RANGE_DISPLAY,
  CLIENT_NAME_DISPLAY,
  INSTANCE_ID_SYMBOL,
  INSTANCE_ID_NAME_DISPLAY,
  WEBSOCKET_NAME_DISPLAY,
  WEBSOCKET_ID_SYMBOL,
  EMPTY_WEBSOCKET_NAME_SYMBOL,
  FILTER_TYPE_INPUT,
  WEBSOCKET_STATUS_SYMBOL,
  WEBSOCKET_STATUS_NAME_DISPLAY,
} from '../constants';

export const NO_TITLE_MAP = {
  [ROLE_SYMBOL]: EMPTY_ROLE_SYMBOL,
  [CLIENT_NAME_SYMBOL]: EMPTY_CLIENT_NAME_SYMBOL,
  [WEBSOCKET_ID_SYMBOL]: EMPTY_WEBSOCKET_NAME_SYMBOL,
};

/* Convert filter map into -> [
 *  {
 *    type: DROPDOWN,
 *    value: OPERATION_TYPE_SYMBOL
 *  }
 * ]
 * Easier to filter and stuff
 * */

export const FILTER_MAP = {
  [TIME_RANGE_SYMBOL]: FILTER_TYPE_DROPDOWN_DEFAULT,
  [ROLE_SYMBOL]: FILTER_TYPE_DROPDOWN,
  [CLIENT_NAME_SYMBOL]: FILTER_TYPE_DROPDOWN,
  [WEBSOCKET_ID_SYMBOL]: FILTER_TYPE_INPUT,
  [WEBSOCKET_STATUS_SYMBOL]: FILTER_TYPE_DROPDOWN,
};

export const GROUP_BY_COLUMNS = [
  ROLE_SYMBOL,
  CLIENT_NAME_SYMBOL,
  INSTANCE_ID_SYMBOL,
];

export const DEFAULT_ORDER_BY = { start_time: 'desc' };

export const TITLE_MAP = {
  [ROLE_SYMBOL]: ROLE_DISPLAY,
  [TIME_RANGE_SYMBOL]: TIME_RANGE_DISPLAY,
  [CLIENT_NAME_SYMBOL]: CLIENT_NAME_DISPLAY,
  [INSTANCE_ID_SYMBOL]: INSTANCE_ID_NAME_DISPLAY,
  [WEBSOCKET_ID_SYMBOL]: WEBSOCKET_NAME_DISPLAY,
  [WEBSOCKET_STATUS_SYMBOL]: WEBSOCKET_STATUS_NAME_DISPLAY,
};

export const defaultColumns = [
  'start_time',
  'websocket_id',
  'websocket_status',
  'open_subscriptions',
  'event_type',
  'instance_uid',
  'client_name',
  'user_role',
  'average_connection_duration',
  'current_open_connections',
  'running_subscriptions',
  'error_code',
  'is_error',
];

export const aliasedColumns = {
  role: 'user_role',
  time: 'start_time',
  error: 'error_code',
  status: 'event_type',
  instance_id: 'instance_uid',
  success: 'is_error',
};

export const actualTypeToAlias = {
  user_role: 'role',
};

export const timeRangeFilters = [
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
];

export const singleSelectFilters = [TIME_RANGE_SYMBOL];
