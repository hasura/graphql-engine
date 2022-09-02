import React from 'react';
import * as RadixCollapsible from '@radix-ui/react-collapsible';
import clsx from 'clsx';
import { FaChevronRight } from 'react-icons/fa';

export type CollapsibleProps = {
  /**
   * Indicates whether the collapse is disabled
   */
  disabled?: boolean;
  /**
   * The collapse trigger children
   */
  triggerChildren: React.ReactNode;
  /**
   * The collapse content children
   */
  children: React.ReactNode;
} & Pick<RadixCollapsible.CollapsibleProps, 'defaultOpen'>;

export const Collapsible: React.VFC<CollapsibleProps> = ({
  triggerChildren,
  children,
  disabled = false,
  defaultOpen = false,
}) => {
  const [open, setOpen] = React.useState(false);

  React.useEffect(() => {
    if (defaultOpen) {
      setOpen(true);
    }
  }, []);

  return (
    <RadixCollapsible.Root
      open={open}
      onOpenChange={setOpen}
      disabled={disabled}
    >
      <RadixCollapsible.Trigger
        className="flex items-center"
        data-testid="collapsible-trigger"
        type="button"
      >
        <FaChevronRight
          className={clsx(
            'transition duration-200 ease-in-out text-gray-600 mr-2',
            open ? 'rotate-90' : 'rotate-0'
          )}
        />
        <span>{triggerChildren}</span>
      </RadixCollapsible.Trigger>
      <RadixCollapsible.Content
        className={clsx(
          ' overflow-hidden',
          open
            ? 'animate-collapsibleContentOpen'
            : 'animate-collapsibleContentClose'
        )}
        data-testid="collapsible-content"
      >
        <div className="my-2 mx-1.5 py-2 px-4 border-solid border-l-2 border-gray-300">
          {children}
        </div>
      </RadixCollapsible.Content>
    </RadixCollapsible.Root>
  );
};
