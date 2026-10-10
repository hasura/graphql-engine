import React, { useState } from 'react';
import * as RadixCollapsible from '@radix-ui/react-collapsible';
import clsx from 'clsx';
import { BsChevronRight } from 'react-icons/bs';
import { Flex, FlexProps } from '@radix-ui/themes';

export type CollapsibleProps = {
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
   * Allows aligning of the RadixCollapsible.Trigger element.
   */
  triggerAlign?: FlexProps['align'];
  /**
   * Disabled wrapping trigger children in a span
   */
  doNotWrapChildren?: boolean;
  /**
   * A way to attach code to openChange handler
   */
  onOpenChange?: (open: boolean) => void;
} & RadixCollapsible.CollapsibleProps;

const Chevron = BsChevronRight;

export const Collapsible: React.FC<CollapsibleProps> = (props) => {
  if (props.onOpenChange) {
    return <CollapsibleInternal {...props} />;
  }

  return <CollapsibleWithState {...props} />;
};

const CollapsibleWithState: React.FC<CollapsibleProps> = ({
  defaultOpen,
  ...rest
}) => {
  const [open, setOpen] = useState(defaultOpen ?? false);

  return <CollapsibleInternal {...rest} open={open} onOpenChange={setOpen} />;
};

const CollapsibleInternal: React.FC<CollapsibleProps> = ({
  triggerChildren,
  children,
  disabled = false,
  chevronClass,
  disableContentStyles = false,
  animationSpeed = 'default',
  triggerClassName,
  triggerAlign = 'center',
  doNotWrapChildren = false,
  open,
  ...rest
}) => {
  return (
    <RadixCollapsible.Root open={open} disabled={disabled} {...rest}>
      <RadixCollapsible.Trigger
        className={triggerClassName}
        data-testid="collapsible-trigger"
        type="button"
      >
        <Flex justify="start" align={triggerAlign} gap="2">
          <Chevron
            className={clsx(
              'transition ease-in-out mt-1',
              open ? 'rotate-90' : 'rotate-0',
              chevronClass,
            )}
          />
          {doNotWrapChildren ? (
            triggerChildren
          ) : (
            <div className="w-full">{triggerChildren}</div>
          )}
        </Flex>
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
              'my-2 mx-1.5 py-2 px-4 border-solid border-l-2 border-gray-300',
          )}
        >
          {children}
        </div>
      </RadixCollapsible.Content>
    </RadixCollapsible.Root>
  );
};
