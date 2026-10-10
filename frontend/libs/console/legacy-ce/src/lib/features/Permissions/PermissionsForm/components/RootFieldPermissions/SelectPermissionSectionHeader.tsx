import React from 'react';
import { Button, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

type Props = {
  onToggle: () => void;
  text: React.ReactElement<any>;
};

export const SelectPermissionSectionHeader: React.FC<Props> = ({
  text,
  onToggle,
}) => (
  <Flex align="center" gap="2">
    <Text>{text}</Text>
    <Button
      mode="default"
      size="sm"
      onClick={onToggle}
      data-test="toggle-all-col-btn"
    >
      Toggle All
    </Button>
  </Flex>
);
