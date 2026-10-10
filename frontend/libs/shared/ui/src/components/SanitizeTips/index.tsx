import { Flex } from '@radix-ui/themes';
import clsx from 'clsx';
import React from 'react';
import { Text } from '../typography';

export const SanitizeTipsMessages: readonly string[] = Object.freeze<string[]>([
  'GraphQL fields are limited to letters, numbers, and underscores.',
  'Any spaces are converted to underscores.',
]);

export const SanitizeTips: React.FC<{
  position?: 'above' | 'below';
  className?: string;
}> = ({ position = 'above', className }) => (
  <Flex
    direction="column"
    gap="1"
    className={clsx(
      position === 'above' && 'mb-4',
      position === 'below' && 'mt-4',
      className && className,
    )}
  >
    {SanitizeTipsMessages.map((m) => (
      <Text as="p" key={m}>
        {m}
      </Text>
    ))}
  </Flex>
);
