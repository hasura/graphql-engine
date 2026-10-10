import React from 'react';
import {
  Box,
  Tooltip as ThemeTooltip,
  TooltipProps as ThemeTooltipProps,
} from '@radix-ui/themes';

export type TooltipProps = ThemeTooltipProps;

export const Tooltip: React.FC<TooltipProps> = ({
  side = 'right',
  align = 'center',
  children,
  ...props
}) => (
  <ThemeTooltip side={side} align={align} {...props}>
    <Box>{children}</Box>
  </ThemeTooltip>
);
