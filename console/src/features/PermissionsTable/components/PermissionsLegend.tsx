import React from 'react';

import { PermissionsIcon } from './PermissionsIcons';

export const PermissionsLegend: React.FC = () => (
  <div className="grid gap-2">
    <div className="flex gap-4">
      <span>
        <PermissionsIcon type="fullAccess" />
        &nbsp;-&nbsp;full access
      </span>
      <span>
        <PermissionsIcon type="noAccess" />
        &nbsp;-&nbsp;no access
      </span>
      <span>
        <PermissionsIcon type="partialAccess" />
        &nbsp;-&nbsp;partial access
      </span>
    </div>
  </div>
);
