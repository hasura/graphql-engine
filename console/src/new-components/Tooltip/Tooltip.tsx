import React from 'react';
import * as Tooltip from '@radix-ui/react-tooltip';
import { FaQuestionCircle } from 'react-icons/fa';

export type TooltipProps = {
  message: string;
  className?: string;
} & Pick<Tooltip.TooltipContentProps, 'side'> &
  Pick<Tooltip.TooltipProps, 'defaultOpen'>;

export const ToolTip: React.VFC<TooltipProps> = ({
  message,
  className,
  side = 'right',
  defaultOpen = false,
}) => (
  <Tooltip.Root delayDuration={0} defaultOpen={defaultOpen}>
    <Tooltip.Trigger className="ml-xs flex">
      <FaQuestionCircle
        className={`h-4 text-muted cursor-pointer ${className}`}
      />
    </Tooltip.Trigger>
    <Tooltip.Content
      side={side}
      className="bg-gray-800 p-sm text-white rounded max-w-lg"
    >
      <Tooltip.Arrow className="fill-current text-gray-800" offset={5} />
      {message}
    </Tooltip.Content>
  </Tooltip.Root>
);
