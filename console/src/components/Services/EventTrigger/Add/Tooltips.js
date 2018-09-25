import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const triggerNameDescription = (
  <Tooltip id="tooltip-trigger-name-description">
    Trigger name can be alphanumeric and can contain underscores
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
    Columns to be sent in the payload of webhook
  </Tooltip>
);

export const postgresDescription = (
  <Tooltip id="tooltip-postgres-description">
    Select the postgres schema and table
  </Tooltip>
);
