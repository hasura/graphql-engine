import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const triggerNameDescription = (
  <Tooltip id="tooltip-trigger-name-description">
    Trigger name can be alphanumeric and can contain underscores and hyphens
  </Tooltip>
);

export const triggerNameDisabled = (
  <Tooltip id="tooltip-trigger-name-disabled-info">
    Trigger name can not be modified
  </Tooltip>
);

export const operationsDescription = (
  <Tooltip id="tooltip-operations-description">
    Listen to these operations on the table
  </Tooltip>
);

export const webhookUrlDescription = (
  <Tooltip id="tooltip-webhook-description">
    POST endpoint which will be triggered with payload on configured events
  </Tooltip>
);

export const advancedOperationDescription = (
  <Tooltip id="tooltip-advanced-operation-description">
    For update triggers, webhook will be triggered only when selected columns
    are modified
  </Tooltip>
);

export const postgresDescription = (
  <Tooltip id="tooltip-postgres-description">
    Select the postgres schema and table
  </Tooltip>
);

export const schemaDisabled = (
  <Tooltip id="tooltip-schema-disabled-info">
    Schema and table names cannot be modified
  </Tooltip>
);
