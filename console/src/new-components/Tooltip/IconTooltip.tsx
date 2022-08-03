import React from 'react';
import { Tooltip, TooltipProps } from '@/new-components/Tooltip';
import { FaQuestionCircle } from 'react-icons/fa';

export type IconTooltipProps = {
  /**
   * The tooltip message
   */
  message: string;
  /**
   * The tooltip icon classes
   */
  className?: string;
} & Pick<TooltipProps, 'side'> &
  Pick<TooltipProps, 'defaultOpen'>;

export const IconTooltip: React.VFC<IconTooltipProps> = ({
  message,
  className,
  side = 'right',
  defaultOpen = false,
}) => (
  <Tooltip
    tooltipContentChildren={message}
    side={side}
    defaultOpen={defaultOpen}
  >
    <FaQuestionCircle
      className={`h-4 text-muted cursor-pointer ${className}`}
    />
  </Tooltip>
);
