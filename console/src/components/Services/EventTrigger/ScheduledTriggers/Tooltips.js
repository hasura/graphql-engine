import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

// TODO: remove these and use <Tooltip> component

export const scheduledTriggerName = (
  <Tooltip id="tooltip-postgres-description">
    Name of the scheduled trigger
  </Tooltip>
);

export const scheduledTriggerWebhook = (
  <Tooltip id="tooltip-postgres-description">Webhook to invoke</Tooltip>
);

export const scheduledTriggerSchedule = (
  <Tooltip id="tooltip-postgres-schedule">Schedule to run</Tooltip>
);

export const scheduledTriggerPayload = (
  <Tooltip id="tooltip-postgres-description">
    Static Payload to be sent to the webhook
  </Tooltip>
);

export const deleteScheduleTrigger = (
  <Tooltip id="tooltip-postgres-description">Delete this trigger</Tooltip>
);
