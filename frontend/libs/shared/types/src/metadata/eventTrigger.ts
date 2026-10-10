import { ActionRequestTransform } from './actions';
import { HeaderConfig } from './header';
import { HeaderFromValue } from './metadata';
import { Table } from './source';

/**
 * NOTE: The metadata type doesn't QUITE match the 'create' arguments here
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger
 */

export type TriggerName = string;
export type Column = string;
export type EventRequestTransform = ActionRequestTransform;
export type EventOperation = 'INSERT' | 'DELETE' | 'UPDATE' | 'MANUAL';

export interface EventTriggerAutoCleanup {
  schedule: string;
  batch_size: number;
  clear_older_than: number;
  timeout: number;
  clean_invocation_logs: boolean;
  paused: boolean;
}

export interface EventTrigger {
  /** Name of the event trigger */
  name: TriggerName;
  /** The SQL function */
  definition: EventTriggerDefinition;
  /** The SQL function */
  retry_conf: RetryConf;
  /** The SQL function */
  webhook?: string;
  webhook_from_env?: string;
  /** The SQL function */
  headers?: HeaderConfig[];
  /** Request transformation object */
  request_transform?: EventRequestTransform;
  /** Auto-cleanup configuration object */
  cleanup_config?: EventTriggerAutoCleanup;
}

export interface EventTriggerDefinition {
  enable_manual: boolean;
  insert?: OperationSpec;
  delete?: OperationSpec;
  update?: OperationSpec;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
 */
export type EventTriggerColumns = '*' | Column[];

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#operationspec
 */
export interface OperationSpec {
  columns: EventTriggerColumns;
  payload?: EventTriggerColumns;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#retryconf
 */
export interface RetryConf {
  /**
   * Number of times to retry delivery.
   * Default: 0
   * @TJS-type integer
   */
  num_retries?: number;
  /**
   * Number of seconds to wait between each retry.
   * Default: 10
   * @TJS-type integer
   */
  interval_sec?: number;
  /**
   * Number of seconds to wait for response before timing out.
   * Default: 60
   * @TJS-type integer
   */
  timeout_sec?: number;
}

// //////////////////////////////
//  #endregion EVENT TRIGGERS
// /////////////////////////////
export type EventTraceContext = {
  sampling_state: string;
  span_id: string;
  trace_id: string;
};

export type EventInvocation<
  T extends Record<string, any> = Record<string, any>,
> = {
  id: string;
  trigger_name: string;
  event_id: string;
  http_status: number;
  request: {
    headers: HeaderFromValue[];
    payload: {
      created_at: string;
      delivery_info: {
        current_retry: number;
        max_retries: number;
      };
      event: {
        data: {
          new: T | null;
          old: T | null;
        };
        op: EventOperation;
        session_variables: Record<string, string>;
        trace_context: EventTraceContext;
      };
      id: string;
      table: Table;
      trigger: {
        name: string;
      };
    };
    version: '1' | '2';
  };
  response: {
    data: {
      message: string;
    };
    type: 'webhook_response' | 'client_error';
    version: '1' | '2';
  };
  created_at: string;
};

export type EventLog<T extends Record<string, any> = Record<string, any>> = {
  id: string;
  schema_name: string;
  table_name: string;
  trigger_name: string;
  payload: {
    data: {
      new: T | null;
      old: T | null;
    };
    op: EventOperation;
    session_variables: Record<string, string>;
    trace_context: EventTraceContext;
  };
  delivered: boolean;
  error: boolean;
  tries: number;
  created_at: string;
  locked: string | null;
  next_retry_at: string | null;
  archived: boolean;
};
