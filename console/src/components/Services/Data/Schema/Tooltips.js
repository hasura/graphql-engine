import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const untrackedTip = (
  <Tooltip id="tooltip-data-service">
    Tables or views that are not exposed over the GraphQL API
  </Tooltip>
);

export const untrackedRelTip = (
  <Tooltip id="tooltip-data-rel-service">
    Relationships inferred via foreign-keys that are not exposed over the
    GraphQL API
  </Tooltip>
);

export const trackableFunctionsTip = (
  <Tooltip id="tooltip-permission-read">
    Custom functions that are not exposed over the GraphQL API
  </Tooltip>
);

export const nonTrackableFunctionsTip = (
  <Tooltip id="tooltip-permission-read">
    Custom functions that do not conform to Hasura requirements
  </Tooltip>
);

export const gqlCompatibilityTip = (
  <Tooltip id="tooltip-scheme-warning">
    This identifier name does not conform to the GraphQL naming standard. Names
    in GraphQL should be limited to this ASCII subset: /[_A-Za-z][_0-9A-Za-z]*/.
  </Tooltip>
);
