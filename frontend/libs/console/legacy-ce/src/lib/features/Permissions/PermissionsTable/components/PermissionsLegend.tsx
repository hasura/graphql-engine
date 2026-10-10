import { Flex, FlexProps } from '@radix-ui/themes';
import { PermissionsIcon } from '@hasura/shared/ui';

export const PermissionsLegend = (props: FlexProps) => (
  <Flex gap="4" {...props}>
    <Flex align="center">
      <PermissionsIcon type="fullAccess" />
      &nbsp;-&nbsp;full access
    </Flex>
    <Flex align="center">
      <PermissionsIcon type="noAccess" />
      &nbsp;-&nbsp;no access
    </Flex>
    <Flex align="center">
      <PermissionsIcon type="partialAccess" />
      &nbsp;-&nbsp;partial access
    </Flex>
  </Flex>
);
