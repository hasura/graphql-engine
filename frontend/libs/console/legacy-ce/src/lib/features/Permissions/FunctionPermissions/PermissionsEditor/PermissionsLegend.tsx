import React from 'react';
import { Flex } from '@radix-ui/themes';
import { PermissionsIcon, Text } from '@hasura/shared/ui';

export const PermissionsLegend: React.FC = () => (
  <Flex gap="4" className="mb-4">
    <Flex gap="1" align="center">
      <PermissionsIcon type="fullAccess" />
      <Text>-&nbsp;full access</Text>
    </Flex>
    <Flex gap="1" align="center">
      <PermissionsIcon type="noAccess" />
      <Text>-&nbsp;no access</Text>
    </Flex>
    <Flex gap="1" align="center">
      <PermissionsIcon type="partialAccess" />
      <Text>-&nbsp;partial access (needs SELECT permissions on table)</Text>
    </Flex>
  </Flex>
);
