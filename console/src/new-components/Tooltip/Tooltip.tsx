import React from 'react';
import * as RadixTooltip from '@radix-ui/react-tooltip';
import clsx from 'clsx';

export type TooltipProps = {
  /**
   * The component children
   */
  children: React.ReactNode;
  /**
   * The tooltip content children
   */
  tooltipContentChildren: React.ReactNode;
  /**
   * The tooltip classes
   */
  className?: string;
} & Pick<RadixTooltip.TooltipContentProps, 'side'> &
  Pick<RadixTooltip.TooltipProps, 'defaultOpen'>;

export const Tooltip: React.VFC<TooltipProps> = ({
  children,
  tooltipContentChildren,
  className,
  side = 'right',
  defaultOpen = false,
}) => (
  <RadixTooltip.Provider>
    <RadixTooltip.Root delayDuration={0} defaultOpen={defaultOpen}>
      <RadixTooltip.Trigger
        id="tooltip-trigger"
        className={clsx('ml-xs inline', className)}
        data-testid="tooltip-trigger"
        type="button"
      >
        {children}
      </RadixTooltip.Trigger>
      <RadixTooltip.Portal>
        <RadixTooltip.Content
          side={side}
          className="bg-gray-800 p-sm text-white rounded max-w-lg z-[102]"
        >
          <RadixTooltip.Arrow
            className="fill-current text-gray-800"
            offset={5}
          />
          {tooltipContentChildren}
        </RadixTooltip.Content>
      </RadixTooltip.Portal>
    </RadixTooltip.Root>
  </RadixTooltip.Provider>
);
