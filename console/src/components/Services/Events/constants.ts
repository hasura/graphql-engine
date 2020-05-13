import { EventTriggerOperation } from './Types';

export const appPrefix = '/events';
export const pageTitle = 'Events';

export const EVENT_TRIGGER_OPERATIONS: EventTriggerOperation[] = [
  'insert',
  'update',
  'delete',
  'enable_manual',
];

export const ADHOC_EVENTS_HEADING = 'Independently Scheduled Events';
