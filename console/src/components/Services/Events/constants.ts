import { EventTriggerOperation, EventKind } from './types';

export const appPrefix = '/events';
export const pageTitle = 'Events';

export const EVENT_TRIGGER_OPERATIONS: EventTriggerOperation[] = [
  'insert',
  'update',
  'delete',
  'enable_manual',
];

export const EVENTS_SERVICE_HEADING = 'Events';
export const ADHOC_EVENTS_HEADING = 'One-off Scheduled Events';
export const CRON_EVENTS_HEADING = 'Cron Triggers';
export const CRON_TRIGGER = 'Cron Trigger';
export const EVENT_TRIGGER = 'Event Trigger';
export const DATA_EVENTS_HEADING = 'Event Triggers';
export const inputStyles =
  'w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';
export const focusYellowRing =
  'focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400';
export const subHeading = 'text-sm font-bold pb-md mt-0 mb-0';
export const heading = 'text-lg font-bold pb-md mt-0 mb-0';

export const getSubserviceHeadings = (subservice: EventKind) => {
  switch (subservice) {
    case 'data':
      return {
        heading: DATA_EVENTS_HEADING,
        triggerHeading: EVENT_TRIGGER,
      };
      break;
    case 'cron':
      return {
        heading: CRON_EVENTS_HEADING,
        triggerHeading: CRON_TRIGGER,
      };
      break;
    case 'scheduled':
      return {
        heading: ADHOC_EVENTS_HEADING,
        triggerHeading: null,
      };
      break;
    default:
      return {
        heading: ADHOC_EVENTS_HEADING,
        triggerHeading: null,
      };
      break;
  }
};
