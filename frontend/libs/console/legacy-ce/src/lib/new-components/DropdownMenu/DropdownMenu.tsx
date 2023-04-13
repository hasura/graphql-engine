import React from 'react';
import * as DropdownMenuPrimitive from '@radix-ui/react-dropdown-menu';
import clsx from 'clsx';

// Styled primitives:

export const DropdownMenuTrigger: React.FC<
  React.ComponentProps<typeof DropdownMenuPrimitive.Trigger>
> = ({ children, ...props }) => (
  <DropdownMenuPrimitive.Trigger asChild {...props}>
    <div className="group">{children}</div>
  </DropdownMenuPrimitive.Trigger>
);

export const DropdownMenuContent: React.FC<
  React.ComponentProps<typeof DropdownMenuPrimitive.Content>
> = ({ children, className, ...props }) => (
  <DropdownMenuPrimitive.Content
    align="start"
    className={clsx(
      'origin-top-left absolute left-0 z-10 mt-xs w-max max-w-xs rounded shadow-md bg-white ring-1 ring-gray-300 divide-y divide-gray-300 focus:outline-none',
      className
    )}
    {...props}
  >
    {children}
  </DropdownMenuPrimitive.Content>
);

export const DropdownMenuItem: React.FC<
  React.ComponentProps<typeof DropdownMenuPrimitive.Item>
> = ({ children, className, ...props }) => (
  <DropdownMenuPrimitive.Item
    className={clsx(
      'cursor-pointer flex items-center mx-1 px-xs py-xs rounded whitespace-nowrap hover:bg-gray-100',
      className
    )}
    {...props}
  >
    {children}
  </DropdownMenuPrimitive.Item>
);

// an implementation of a dropdownmenu.
// for more flexibility, such as being able to use labels, combine the styled components with other primatives from radix.

export interface DropdownMenuProps {
  options?: {
    root?: React.ComponentProps<typeof DropdownMenuPrimitive.Root>;
    trigger?: React.ComponentProps<typeof DropdownMenuPrimitive.Trigger>;
    content?: React.ComponentProps<typeof DropdownMenuPrimitive.Content>;
    item?: React.ComponentProps<typeof DropdownMenuPrimitive.Item>;
    portal?: React.ComponentProps<typeof DropdownMenuPrimitive.Portal>;
  };
  items: React.ReactNode[][];
  zIndex?: string;
}

export const DropdownMenu: React.FC<DropdownMenuProps> = ({
  children,
  items,
  options,
  zIndex = 'z-[101]',
}) => (
  <DropdownMenuPrimitive.Root {...options?.root}>
    <DropdownMenuTrigger {...options?.trigger}>{children}</DropdownMenuTrigger>
    <DropdownMenuPrimitive.Portal {...options?.portal}>
      <DropdownMenuPrimitive.Content
        align="start"
        {...options?.content}
        className={zIndex}
      >
        <div className="z-10 mt-xs w-max rounded shadow-md bg-white ring-1 ring-gray-300 divide-y divide-gray-300 focus:outline-none">
          {items.map((group, groupIndex) => (
            <div className="py-1" key={groupIndex}>
              {group.map((item, itemIndex) => (
                <DropdownMenuPrimitive.Item key={itemIndex} {...options?.item}>
                  <div className="cursor-pointer flex items-center mx-1 px-xs rounded whitespace-nowrap hover:bg-gray-100">
                    {item}
                  </div>
                </DropdownMenuPrimitive.Item>
              ))}
            </div>
          ))}
        </div>
      </DropdownMenuPrimitive.Content>
    </DropdownMenuPrimitive.Portal>
  </DropdownMenuPrimitive.Root>
);
