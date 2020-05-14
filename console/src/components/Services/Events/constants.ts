import { EventTriggerOperation } from './types';

export const appPrefix = '/events';
export const pageTitle = 'Events';

export const EVENT_TRIGGER_OPERATIONS: EventTriggerOperation[] = [
  'insert',
  'update',
  'delete',
  'enable_manual',
];

export const EVENTS_SERVICE_HEADING = 'Events';
export const ADHOC_EVENTS_HEADING = 'Independent Scheduled Events';
export const CRON_EVENTS_HEADING = 'Cron Events';
export const CRON_TRIGGER = 'Cron Trigger';
export const EVENT_TRIGGER = 'Event Trigger';
export const DATA_EVENTS_HEADING = 'Data Events';
