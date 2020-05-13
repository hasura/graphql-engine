import { Header as ServerHeader } from '../../Common/utils/v1QueryUtils';
import { Nullable } from '../../Common/utils/tsUtils';

export const LOADING_TRIGGERS = 'Events/LOADING_TRIGGERS';
export const LOADED_TRIGGERS = 'Events/LOADED_TRIGGERS';
export const LOADING_SCHEDULED_TRIGGERS = 'Events/LOADING_SCHEDULED_TRIGGERS';
export const LOADED_SCHEDULED_TRIGGERS = 'Events/LOADED_SCHEDULED_TRIGGERS';
export const LOADING_EVENT_TRIGGERS = 'Events/LOADING_EVENT_TRIGGERS';
export const LOADED_EVENT_TRIGGERS = 'Events/LOADED_EVENT_TRIGGERS';
export const LOADING_TRIGGERS_ERROR = 'Events/LOADING_TRIGGERS_ERROR';
export const SET_CURRENT_TRIGGER = 'Events/SET_CURRENT_TRIGGER';
export const LOAD_PENDING_DATA_EVENTS = 'Events/LOAD_PENDING_DATA_EVENTS';

/*
 * Common types for events service
 */

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
  num_retries: number;
  interval_sec: number;
  timeout_sec: number;
  tolerance_sec: Nullable<number>;
};

export type TriggerEventsProps = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

export type VoidCallback = () => void;

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
  comment: string | null;
  configuration: {
    definition: Record<EventTriggerOperation, EventTriggerOperationDefinition>;
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
    num_retries: number;
    retry_interval_seconds: number;
    timeout_seconds: number;
    tolerance_seconds: number;
  };
  include_in_metadata: boolean;
  comment: Nullable<string>;
};

/*
 * Redux State Type for Event and Scheduled Triggers
 */
export type Triggers = {
  scheduled: ScheduledTrigger[];
  event: EventTrigger[];
};

/*
 * Redux Action types
 */

export type RASetAllTriggers = {
  type: typeof LOADED_TRIGGERS;
  data: Triggers;
};

export type RASetScheduledTriggers = {
  type: typeof LOADED_SCHEDULED_TRIGGERS;
  data: ScheduledTrigger[];
};

export type RASetEventTriggers = {
  type: typeof LOADED_EVENT_TRIGGERS;
  data: EventTrigger[];
};

export type RASetCurrentTrigger = {
  type: typeof SET_CURRENT_TRIGGER;
  name: string;
};

export type TriggerKind = 'event' | 'scheduled';

export type RATriggers =
  | RASetAllTriggers
  | RASetScheduledTriggers
  | RASetEventTriggers
  | RASetCurrentTrigger;
