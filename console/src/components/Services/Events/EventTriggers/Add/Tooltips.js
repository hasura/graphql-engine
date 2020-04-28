import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const manualTriggerInfo = (
  <Tooltip id="tooltip-manual-trigger-description">
    Selecting this option will let you invoke this trigger from Data Broeser
  </Tooltip>
);

export const triggerNameDescription = (
  <Tooltip id="tooltip-trigger-name-description">
    Trigger name can be alphanumeric and can contain underscores and hyphens
  </Tooltip>
);

export const operationsDescription = (
  <Tooltip id="tooltip-operations-description">
    Trigger event on these table operations
  </Tooltip>
);

export const manualOperationsDescription = (
  <Tooltip id="tooltip-manual-operations-description">
    Trigger manually from table data browser in console
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
