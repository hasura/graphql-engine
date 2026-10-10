import { Heading } from '@radix-ui/themes';
import React from 'react';

export const SectionHeader: React.FC<{ children?: React.ReactNode }> = ({
  children,
}) => <Heading size="3">{children}</Heading>;
