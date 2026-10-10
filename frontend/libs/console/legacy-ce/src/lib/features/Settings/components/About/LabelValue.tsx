import * as React from 'react';
import { Flex } from '@radix-ui/themes';

export const LabelValue: React.FC<{
  label: React.ReactNode;
  value: React.ReactNode;
}> = (props) => {
  const { label, value } = props;
  return (
    <Flex direction="column">
      <b className="text-muted">{label}: </b>
      <span className="text-muted font-normal">{value}</span>
    </Flex>
  );
};
