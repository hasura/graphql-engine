import { Action as ReduxAction } from 'redux';
import { RouteComponentProps } from 'react-router';
import { Nullable } from '../../Common/utils/tsUtils';
import { Dispatch } from '../../../types';
import { ServerHeader } from '../../../metadata/types';

export const LOADING_TRIGGERS = 'Events/LOADING_TRIGGERS';
export const LOADED_TRIGGERS = 'Events/LOADED_TRIGGERS';
export const LOADED_SCHEDULED_TRIGGERS = 'Events/LOADED_SCHEDULED_TRIGGERS';
export const SET_CURRENT_TRIGGER = 'Events/SET_CURRENT_TRIGGER';
export const LOAD_PENDING_DATA_EVENTS = 'Events/LOAD_PENDING_DATA_EVENTS';

/*
 * Common types for events service
 */

export type Event = {
  id: string;
  payload: string;
  webhook_conf?: string;
  comment?: string;
};

export type RouterTriggerProps = RouteComponentProps<
  {
    triggerName: string;
  },
  unknown
>;

export type TriggerKind = 'event' | 'cron';

export type EventKind = 'data' | 'cron' | 'scheduled';

export type URLType = 'static' | 'env';

export type URLConf = {
  type: URLType;
  value: string;
};

export type ServerWebhookConf =
  | string
  | {
      from_env: string;
    };
export type RetryConf = {
  num_retries?: number;
  interval_sec?: number;
  timeout_sec?: number;
};

export type TriggerEventsProps = {
  dispatch: Dispatch;
  currentTrigger?: ScheduledTrigger;
};

export type VoidCallback = () => void;

export type InvocationLog = {
  event_id: string;
  id: string;
  status: number;
  created_at: string;
  request: string;
  response: string;
};

/*
 * Types related to Event Triggers
 */

export type EventTriggerOperation =
  | 'insert'
  | 'update'
  | 'delete'
  | 'enable_manual';

export type ETOperationColumn = {
  name: string;
  type: string;
  enabled: boolean;
};

export type EventTriggerOperationDefinition = {
  columns: string[] | '*';
};

export type EventTrigger = {
  name: string;
  table_name: string;
  schema_name: string;
  source: string;
  comment: string | null;
  configuration: {
    definition: {
      [key in EventTriggerOperation]: EventTriggerOperationDefinition;
    };
    headers: ServerHeader[];
    retry_conf: RetryConf;
    webhook: Nullable<string>;
    webhook_from_env?: Nullable<string>;
  };
};

/*
 * Types related to Scheduled Triggers
 */

export type ScheduledTrigger = {
  name: string;
  header_conf: ServerHeader[];
  payload: any;
  webhook_conf: string;
  cron_schedule: string;
  retry_conf: {
    num_retries?: number;
    retry_interval_seconds?: number;
    timeout_seconds?: number;
    tolerance_seconds?: number;
  };
  include_in_metadata: boolean;
  comment: Nullable<string>;
};

/*
 * Redux State Type for Event and Scheduled Triggers
 */
export type Triggers = ScheduledTrigger[];

/*
 * Redux Action types
 */

export interface RASetCurrentTrigger extends ReduxAction {
  type: typeof SET_CURRENT_TRIGGER;
  name: string;
}

export type RAEvents = RASetCurrentTrigger | { type: typeof LOADING_TRIGGERS };
