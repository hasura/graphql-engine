import React from 'react';
import clsx from 'clsx';

export type BadgeColor =
  | 'green'
  | 'red'
  | 'yellow'
  | 'indigo'
  | 'gray'
  | 'blue'
  | 'purple';
interface BadgeProps extends React.ComponentProps<'span'> {
  /**
   * The color of the basge
   */
  color?: BadgeColor;
}

const badgeClassnames: Record<BadgeColor, string> = {
  green: 'bg-green-100 text-green-800',
  red: 'bg-red-100 text-red-800',
  yellow: 'bg-yellow-100 text-yellow-800',
  gray: 'bg-gray-300 text-gray-800',
  indigo: 'bg-indigo-100 text-indigo-800',
  blue: 'bg-blue-100 text-blue-800',
  purple: 'bg-purple-100 text-purple-800',
};

export const Badge: React.FC<React.PropsWithChildren<BadgeProps>> = ({
  color = 'gray',
  children,
  ...rest
}) => {
  return (
    <span
      data-testid="badge"
      {...rest}
      className={clsx(
        'inline-flex items-center px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold',
        badgeClassnames[color],
        rest.className,
        rest.onClick && 'cursor-pointer'
      )}
    >
      {children}
    </span>
  );
};
