import React from 'react';
import { Flex } from '@radix-ui/themes';
import { PermissionsIcon } from '@hasura/shared/ui';

export const PermissionsLegend: React.FC = () => (
  <Flex gap="4">
    <span>
      <PermissionsIcon type="fullAccess" />
      &nbsp;-&nbsp;allowed
    </span>
    <span>
      <PermissionsIcon type="noAccess" />
      &nbsp;-&nbsp;not allowed
    </span>
  </Flex>
);
