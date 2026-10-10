import { IconTooltip, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import React from 'react';

export const SectionHeader: React.FC<{ header: string; tip: string }> = ({
  header,
  tip,
}) => (
  <div>
    <Flex align="center" className="my-4" gap="2">
      <Text weight="medium">{header}</Text>
      <IconTooltip message={tip} />
    </Flex>
  </div>
);
