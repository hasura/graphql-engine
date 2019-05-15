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

export const trackableFunctions = (
  <Tooltip id="tooltip-permission-read">
    Custom functions that are not exposed over the GraphQL API
  </Tooltip>
);

// export const nonTrackableFunctions = (
//   <Tooltip id="tooltip-permission-read">WIP</Tooltip>
// );
