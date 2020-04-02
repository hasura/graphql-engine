import { Header as ServerHeader } from '../../Common/utils/v1QueryUtils';

export const LOADING_TRIGGERS = 'Triggers/LOADING_TRIGGERS';
export const LOADED_TRIGGERS = 'Triggers/LOADED_TRIGGERS';
export const LOADING_SCHEDULED_TRIGGERS = 'Triggers/LOADING_SCHEDULED_TRIGGERS';
export const LOADED_SCHEDULED_TRIGGERS = 'Triggers/LOADED_SCHEDULED_TRIGGERS';
export const LOADING_EVENT_TRIGGERS = 'Triggers/LOADING_EVENT_TRIGGERS';
export const LOADED_EVENT_TRIGGERS = 'Triggers/LOADED_EVENT_TRIGGERS';
export const LOADING_TRIGGERS_ERROR = 'Triggers/LOADING_TRIGGERS_ERROR';
export const SET_CURRENT_TRIGGER = 'Triggers/SET_CURRENT_TRIGGER';

export type WebhookConf =
  | string
  | {
      from_env: string;
    };

export type ScheduleTriggerType = 'cron' | 'adhoc';

export type ScheduleConf = {
  type: ScheduleTriggerType;
  value: string;
};

export type ScheduledTrigger = {
  name: string;
  header_conf: ServerHeader[];
  payload: any;
  webhook_conf: WebhookConf;
  schedule_conf: ScheduleConf;
};

export type EventTrigger = {
  name: string;
};

export type Triggers = {
  scheduled: ScheduledTrigger[];
  event: EventTrigger[];
};

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

export const EVENT_TRIGGER_IDENTIFIER = 'event';
export const SCHEDULED_TRIGGER_IDENTIFIER = 'scheduled';
export type TriggerKind =
  | typeof EVENT_TRIGGER_IDENTIFIER
  | typeof SCHEDULED_TRIGGER_IDENTIFIER;

export type RATriggers =
  | RASetAllTriggers
  | RASetScheduledTriggers
  | RASetEventTriggers
  | RASetCurrentTrigger;
