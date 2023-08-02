import React from 'react';
import * as RadixCollapsible from '@radix-ui/react-collapsible';
import clsx from 'clsx';
import { BsChevronRight } from 'react-icons/bs';

export type CollapsibleProps = {
  /**
   * Indicates whether the collapse is disabled
   */
  disabled?: boolean;
  /**
   * Allows styling to chevron icon
   */
  chevronClass?: string;
  /**
   * The collapse trigger children
   */
  triggerChildren: React.ReactNode;
  /**
   * The collapse content children
   */
  children: React.ReactNode;
  /**
   * Disables content styles (border, padding, margin)
   */
  disableContentStyles?: boolean;
  /**
   *  Collapsible animation duration
   */
  animationSpeed?: 'default' | 'fast';
  /**
   * Allows styling of the RadixCollapsible.Trigger element. e.g. add a background color that includes the chevron + children
   */
  triggerClassName?: string;
  /**
   * Disabled wrapping trigger children in a span
   */
  doNotWrapChildren?: boolean;
  /**
   * A way to attach code to openChange handler
   */
  onOpenChange?: (open: boolean) => void;
} & Pick<RadixCollapsible.CollapsibleProps, 'defaultOpen'>;

export const Collapsible: React.VFC<CollapsibleProps> = ({
  triggerChildren,
  children,
  disabled = false,
  chevronClass,
  defaultOpen = false,
  disableContentStyles = false,
  animationSpeed = 'default',
  triggerClassName,
  doNotWrapChildren = false,
  onOpenChange,
}) => {
  const [open, setOpen] = React.useState(false);

  const Chevron = BsChevronRight;

  React.useEffect(() => {
    if (defaultOpen) {
      setOpen(true);
    }
  }, []);

  return (
    <RadixCollapsible.Root
      open={open}
      onOpenChange={open => {
        setOpen(open);
        onOpenChange?.(open);
      }}
      disabled={disabled}
    >
      <RadixCollapsible.Trigger
        className={clsx('flex items-center', triggerClassName)}
        data-testid="collapsible-trigger"
        type="button"
      >
        <Chevron
          className={clsx(
            'transition ease-in-out text-gray-600',
            open ? 'rotate-90' : 'rotate-0',
            chevronClass ? chevronClass : 'mr-2'
          )}
        />
        {doNotWrapChildren ? triggerChildren : <span>{triggerChildren}</span>}
      </RadixCollapsible.Trigger>
      <RadixCollapsible.Content
        className={clsx(' overflow-hidden', open, {
          'animate-collapsibleContentOpen':
            open && animationSpeed === 'default',
          'animate-collapsibleContentOpenFast':
            open && animationSpeed !== 'default',
          'animate-collapsibleContentClose':
            !open && animationSpeed === 'default',
          'animate-collapsibleContentCloseFast':
            !open && animationSpeed !== 'default',
        })}
        data-testid="collapsible-content"
      >
        <div
          className={clsx(
            !disableContentStyles &&
              'my-2 mx-1.5 py-2 px-4 border-solid border-l-2 border-gray-300'
          )}
        >
          {children}
        </div>
      </RadixCollapsible.Content>
    </RadixCollapsible.Root>
  );
};
