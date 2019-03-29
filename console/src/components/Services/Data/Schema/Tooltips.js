import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';

export const dataAPI = (
  <Tooltip id="tooltip-data-service">
    To use Data APIs for complex joins/queries, create a view and then add the
    view here.
  </Tooltip>
);

export const untrackedTip = (
  <Tooltip id="tooltip-data-service">
    Tables or views that are not exposed over GraphQL
  </Tooltip>
);

export const untrackedRelTip = (
  <Tooltip id="tooltip-data-rel-service">
    Foreign keys between tracked tables that are not relationships
  </Tooltip>
);

export const quickDefaultPublic = (
  <Tooltip id="tooltip-permission-public">
    The selected role can perform select, insert, update and delete on all rows
    of the table.
  </Tooltip>
);

export const quickDefaultReadOnly = (
  <Tooltip id="tooltip-permission-read">
    The selected role can perform select on all rows of the table.
  </Tooltip>
);

export const trackableFunctions = (
  <Tooltip id="tooltip-permission-read">
    Track supported functions to query them with GraphQL
  </Tooltip>
);

export const nonTrackableFunctions = (
  <Tooltip id="tooltip-permission-read">WIP</Tooltip>
);
