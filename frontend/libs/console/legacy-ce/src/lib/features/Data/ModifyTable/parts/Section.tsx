import { IconTooltip } from '@hasura/shared/ui';
import { Flex, Heading } from '@radix-ui/themes';
import clsx from 'clsx';
import React from 'react';

export const Section: React.FC<{
  className?: string;
  headerText: React.ReactNode;
  tooltipMessage?: string;
  children?: React.ReactNode;
}> = ({ className, children, headerText, tooltipMessage }) => (
  <div className={clsx('mb-4', className)}>
    <Flex direction="row" align="center" gap="2" className="mb-2">
      <Heading size="3">{headerText}</Heading>
      {!!tooltipMessage && <IconTooltip message={tooltipMessage} />}
    </Flex>
    {children}
  </div>
);
