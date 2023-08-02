import {
  FILTER_TYPE_INPUT,
  WORKER_ID_DISPLAY,
  OPERATION_NAME_SYMBOL,
  OPERATION_NAME_DISPLAY,
  FILTER_TYPE_DROPDOWN,
  SUBSCRIPTION_STATUS_SYMBOL,
  SUBSCRIPTION_STATUS_NAME_DISPLAY,
  TIME_RANGE_SYMBOL,
  FILTER_TYPE_DROPDOWN_DEFAULT,
  TIME_RANGE_DISPLAY,
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
} from '../constants';

export const NO_TITLE_MAP = {};

/* Convert filter map into -> [
 *  {
 *    type: DROPDOWN,
 *    value: OPERATION_TYPE_SYMBOL
 *  }
 * ]
 * Easier to filter and stuff
 * */

const WORKER_ID_SYMBOL = 'poller_ids';

export const FILTER_MAP = {
  [TIME_RANGE_SYMBOL]: FILTER_TYPE_DROPDOWN_DEFAULT,
  [SUBSCRIPTION_STATUS_SYMBOL]: FILTER_TYPE_DROPDOWN,
  [OPERATION_NAME_SYMBOL]: FILTER_TYPE_INPUT,
  [WORKER_ID_SYMBOL]: FILTER_TYPE_INPUT,
};

export const START_TIME_COLUMN = 'min_time';
export const DEFAULT_ORDER_BY = { [START_TIME_COLUMN]: 'desc' };

export const TITLE_MAP = {
  [TIME_RANGE_SYMBOL]: TIME_RANGE_DISPLAY,
  [SUBSCRIPTION_STATUS_SYMBOL]: SUBSCRIPTION_STATUS_NAME_DISPLAY,
  [OPERATION_NAME_SYMBOL]: OPERATION_NAME_DISPLAY,
  [WORKER_ID_SYMBOL]: WORKER_ID_DISPLAY,
};

export const defaultColumns = [
  START_TIME_COLUMN,
  'status',
  'operation_name',
  'user_role',
  'session_variables',
  'variables',
  'total_subscribers',
  'last_execution_time',
  'execution_batch_size',
  'poller_id',
];

export const defaultHeaders = [
  'started',
  'status',
  'operation_name',
  'role',
  'session_variables',
  'query_variables',
  'no_of_subscribers',
  'last_execution_time',
  'no_of_postgres_queries',
  'worker_id',
];

export const aliasedColumns = {
  started: START_TIME_COLUMN,
  role: 'user_role',
  query_variables: 'variables',
  worker_id: 'poller_id',
  no_of_subscribers: 'total_subscribers',
  no_of_postgres_queries: 'execution_batch_size',
};

export const actualTypeToAlias = {
  user_role: 'role',
};

export const singleSelectFilters = [TIME_RANGE_SYMBOL];

export const timeRangeFilters = [
  TIME_RANGE_BY_HOUR,
  TIME_RANGE_BY_6_HOURS,
  TIME_RANGE_BY_12_HOURS,
  TIME_RANGE_BY_DAY,
];

export const REFETCH_DELAY = 1;
export const EXECUTION_TIME_DIVIDER_CONSTANT = 2;
export const BATCH_SIZE_CONSTANT = 100;
