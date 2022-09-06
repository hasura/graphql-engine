import React from 'react';
import clsx from 'clsx';

type BadgeColor = 'green' | 'red' | 'yellow' | 'indigo' | 'gray';
interface BadgeProps extends React.ComponentProps<'span'> {
  /**
   * The color of the basge
   */
  color: BadgeColor;
}

const badgeClassnames: Record<BadgeColor, string> = {
  green: 'bg-green-100 text-green-800',
  red: 'bg-red-100 text-red-800',
  yellow: 'bg-yellow-100 text-yellow-800',
  gray: 'bg-gray-100 text-gray-800',
  indigo: 'bg-indigo-100 text-indigo-800',
};

export const Badge: React.FC<React.PropsWithChildren<BadgeProps>> = props => {
  const { color, children, ...rest } = props;

  return (
    <span
      {...rest}
      className={clsx(
        'inline-flex items-center mr-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold',
        badgeClassnames[color],
        rest.className,
        rest.onClick && 'cursor-pointer'
      )}
    >
      {children}
    </span>
  );
};
