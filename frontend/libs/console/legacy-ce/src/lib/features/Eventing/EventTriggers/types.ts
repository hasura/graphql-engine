import { UsePaginationState } from '@hasura/shared/hooks';
import { type ClientHeader, defaultHeader, Table } from '@hasura/shared/types';

/*
 * Common types for events service
 */

export type Event = {
  id: string;
  payload: string;
  webhook_conf?: string;
  comment?: string;
};

export type URLType = 'static' | 'env';

export type URLConf = {
  type: URLType;
  value: string;
};

export type RetryConf = {
  num_retries?: number;
  interval_sec?: number;
  timeout_sec?: number;
};

/*
 * Types related to Event Triggers
 */

export type EventTriggerOperation = 'INSERT' | 'UPDATE' | 'DELETE' | 'MANUAL';

export type ETOperationColumn = {
  name: string;
  type: string;
  enabled: boolean;
};

export type EventTriggerAutoCleanup = {
  schedule?: string;
  batch_size?: number;
  clear_older_than?: number;
  timeout?: number;
  clean_invocation_logs?: boolean;
  paused?: boolean;
};

export type FilterTableProps = UsePaginationState & {
  rows: any[];
  count?: number;
  columns: string[];
};

export type LocalEventTriggerState = {
  name: string;
  table: Table | null;
  operations: EventTriggerOperation[];
  operationColumns: string[];
  webhook: URLConf;
  retryConf: RetryConf;
  headers: ClientHeader[];
  source: string;
  isAllColumnChecked: boolean;
  cleanupConfig?: EventTriggerAutoCleanup;
};

export const defaultState: LocalEventTriggerState = {
  name: '',
  table: null,
  operations: [],
  operationColumns: [],
  webhook: {
    type: 'static',
    value: '',
  },
  retryConf: {
    num_retries: 0,
    interval_sec: 10,
    timeout_sec: 60,
  },
  headers: [{ ...defaultHeader }],
  source: '',
  isAllColumnChecked: true,
};
