import React from 'react';
import clsx from 'clsx';
import {
  Badge as ThemeBadge,
  type BadgeProps as ThemeBadgeProps,
} from '@radix-ui/themes';

export type BadgeColor = Exclude<ThemeBadgeProps['color'], undefined>;

interface BadgeProps extends ThemeBadgeProps {}

const badgeColorMap = {
  green: { color: 'green', variant: 'soft' },
  red: { color: 'red', variant: 'soft' },
  yellow: { color: 'yellow', variant: 'soft' },
  gray: { color: 'gray', variant: 'soft' },
  indigo: { color: 'indigo', variant: 'soft' },
  blue: { color: 'blue', variant: 'soft' },
  purple: { color: 'purple', variant: 'soft' },
};

export const Badge: React.FC<React.PropsWithChildren<BadgeProps>> = ({
  color = 'gray',
  children,
  ...rest
}) => {
  const { color: themeColor, variant } = badgeColorMap[
    color as keyof typeof badgeColorMap
  ] ?? { color };

  return (
    <ThemeBadge
      color={themeColor as BadgeColor}
      variant={variant as ThemeBadgeProps['variant']}
      {...rest}
      className={clsx(rest.className, rest.onClick && 'cursor-pointer')}
    >
      {children}
    </ThemeBadge>
  );
};
