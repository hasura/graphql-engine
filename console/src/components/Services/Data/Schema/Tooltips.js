import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const untrackedTablesTip = (
  <Tooltip id="tooltip-tables-untracked">
    Tables or views that are not exposed over the GraphQL API
  </Tooltip>
);

export const untrackedRelTip = (
  <Tooltip id="tooltip-relationships-untracked">
    Relationships inferred via foreign-keys that are not exposed over the
    GraphQL API
  </Tooltip>
);

export const trackableFunctionsTip = (
  <Tooltip id="tooltip-functions-untracked">
    Custom functions that are not exposed over the GraphQL API
  </Tooltip>
);

export const nonTrackableFunctionsTip = (
  <Tooltip id="tooltip-functions-untrackable">
    Custom functions that do not conform to Hasura requirements
  </Tooltip>
);
