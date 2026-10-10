import { EventTriggerOperation } from './types';

export const pageTitle = 'Events';

export const EVENT_TRIGGER_OPERATIONS: EventTriggerOperation[] = [
  'INSERT',
  'UPDATE',
  'DELETE',
  'MANUAL',
];

export const triggerNameDescription =
  'Trigger name can be alphanumeric, can contain underscores and hyphens, and must be at most 42 characters.';

export const triggerNameSource = 'Select the database';

export const operationsDescription = 'Trigger event on these table operations';

export const webhookUrlDescription =
  'Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url';

export const advancedOperationDescription =
  'For update triggers, webhook will be triggered only when selected columns are modified';

export const postgresDescription = 'Select the database schema and table';
