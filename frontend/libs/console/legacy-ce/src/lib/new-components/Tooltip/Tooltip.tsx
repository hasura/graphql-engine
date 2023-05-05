import React from 'react';
import * as RadixTooltip from '@radix-ui/react-tooltip';
import clsx from 'clsx';

type Theme = 'dark' | 'light';

export const themes: Record<Theme, string> = {
  dark: 'bg-gray-800 text-white',
  light: 'bg-white text-gray-800 border border-gray-800',
};

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
  /**
   * The theme of the tooltip
   */
  theme?: Theme;
  /**  The radix tooltip options */
  options?: {
    provider?: RadixTooltip.TooltipProviderProps;
    root?: RadixTooltip.TooltipProps;
    trigger?: RadixTooltip.TooltipTriggerProps;
    portal?: RadixTooltip.TooltipPortalProps;
    content?: RadixTooltip.TooltipContentProps;
    arrow?: RadixTooltip.TooltipArrowProps;
  };
} & Pick<RadixTooltip.TooltipContentProps, 'side' | 'align'> &
  Pick<RadixTooltip.TooltipProps, 'defaultOpen'>;

export const Tooltip: React.VFC<TooltipProps> = ({
  children,
  tooltipContentChildren,
  className,
  side = 'right',
  align = 'center',
  defaultOpen = false,
  options = {},
  theme = 'dark',
}) => (
  <RadixTooltip.Provider {...options?.provider}>
    <RadixTooltip.Root
      delayDuration={0}
      defaultOpen={defaultOpen}
      {...options?.root}
    >
      <RadixTooltip.Trigger
        id="tooltip-trigger"
        className={clsx('ml-xs inline', className)}
        data-testid="tooltip-trigger"
        type="button"
        {...options?.trigger}
      >
        {children}
      </RadixTooltip.Trigger>
      <RadixTooltip.Portal {...options?.portal}>
        <RadixTooltip.Content
          side={side}
          align={align}
          className={clsx(themes[theme], 'p-sm rounded max-w-lg z-[102]')}
          {...options?.content}
        >
          <RadixTooltip.Arrow
            className="fill-current text-gray-800"
            offset={5}
            {...options?.arrow}
          />
          {tooltipContentChildren}
        </RadixTooltip.Content>
      </RadixTooltip.Portal>
    </RadixTooltip.Root>
  </RadixTooltip.Provider>
);
