import React from 'react';
import * as DropdownMenuPrimitive from '@radix-ui/react-dropdown-menu';

interface DropdownMenuProps {
  options?: {
    root?: React.ComponentProps<typeof DropdownMenuPrimitive.Root>;
    trigger?: React.ComponentProps<typeof DropdownMenuPrimitive.Trigger>;
    content?: React.ComponentProps<typeof DropdownMenuPrimitive.Content>;
    item?: React.ComponentProps<typeof DropdownMenuPrimitive.Item>;
    portal?: React.ComponentProps<typeof DropdownMenuPrimitive.Portal>;
  };
  items: React.ReactNode[][];
}

export const DropdownMenu: React.FC<DropdownMenuProps> = ({
  children,
  items,
  options,
}) => (
  <DropdownMenuPrimitive.Root {...options?.root}>
    <DropdownMenuPrimitive.Trigger asChild {...options?.trigger}>
      <div className="group">{children}</div>
    </DropdownMenuPrimitive.Trigger>
    <DropdownMenuPrimitive.Portal {...options?.portal}>
      <DropdownMenuPrimitive.Content align="start" {...options?.content}>
        <div className="origin-top-left absolute left-0 z-10 mt-xs w-max max-w-xs rounded shadow-md bg-white ring-1 ring-gray-300 divide-y divide-gray-300 focus:outline-none">
          {items.map(group => (
            <div className="py-1">
              {group.map(item => (
                <DropdownMenuPrimitive.Item {...options?.item}>
                  <div className="cursor-pointer flex items-center mx-1 px-xs py-xs rounded whitespace-nowrap hover:bg-gray-100">
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
