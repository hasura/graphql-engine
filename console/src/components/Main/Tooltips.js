import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import globals from '../../Globals';

export const data = (
  <Tooltip id="tooltip-data-service">Data & Schema management</Tooltip>
);

export const apiexplorer = (
  <Tooltip id="tooltip-api-explorer">Test the GraphQL APIs</Tooltip>
);

export const events = (
  <Tooltip id="tooltip-events">Manage Event Triggers</Tooltip>
);

export const remoteSchema = (
  <Tooltip id="tooltip-remoteschema">Manage Remote Schemas</Tooltip>
);

export const secureEndpoint = (
  <Tooltip id="tooltip-secure-endpoint">
    This graphql endpoint is public and you should add an{' '}
    {globals.adminSecretLabel}
  </Tooltip>
);
