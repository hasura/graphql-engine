import clsx from 'clsx';
import React from 'react';

const gqlRegex = new RegExp('[^A-Za-z0-9_]', 'g');

export const sanitizeGraphQLFieldNames = (value: string): string => {
  return value.replace(' ', '_').replace(gqlRegex, '');
};

export const SanitizeTipsMessages: readonly string[] = Object.freeze<string[]>([
  'GraphQL fields are limited to letters, numbers, and underscores.',
  'Any spaces are converted to underscores.',
]);

export const SanitizeTips: React.VFC<{
  position?: 'above' | 'below';
  className?: string;
}> = ({ position = 'above', className }) => (
  <div
    className={clsx(
      'flex text-muted flex-col',
      position === 'above' && 'mb-4',
      position === 'below' && 'mt-4',
      className && className
    )}
  >
    {SanitizeTipsMessages.map(m => (
      <div key={m}>{m}</div>
    ))}
  </div>
);
