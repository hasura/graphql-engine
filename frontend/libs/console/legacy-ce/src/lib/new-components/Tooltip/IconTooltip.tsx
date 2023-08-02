import React, { ReactElement, ReactNode } from 'react';
import { Tooltip, TooltipProps } from '.';
import { FaQuestionCircle } from 'react-icons/fa';

export type IconTooltipProps = {
  /**
   * The tooltip message
   */
  message: React.ReactNode;
  /**
   * The tooltip icon classes
   */
  className?: string;
  /**
   * tooltip icon other then ?
   */
  icon?: ReactNode;
} & Pick<TooltipProps, 'side'> &
  Pick<TooltipProps, 'defaultOpen'>;

export const IconTooltip: React.VFC<IconTooltipProps> = ({
  message,
  className,
  side = 'right',
  defaultOpen = false,
  icon,
}) => (
  <Tooltip
    tooltipContentChildren={message}
    side={side}
    defaultOpen={defaultOpen}
    className="flex items-center"
  >
    {!icon ? (
      <FaQuestionCircle
        className={`h-4 text-muted cursor-pointer ${className}`}
      />
    ) : (
      icon
    )}
  </Tooltip>
);
