import React from 'react';
import { Flex } from '@radix-ui/themes';
import { PermissionsIcon } from '@hasura/shared/ui';

export const PermissionsLegend: React.FC = () => (
  <Flex gap="4" className="mb-4">
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
      &nbsp;-&nbsp;partial access (needs SELECT permissions on table)
    </span>
  </Flex>
);
