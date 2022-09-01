import React from 'react';
import * as DropdownMenuPrimitive from '@radix-ui/react-dropdown-menu';
import { Button } from '@/new-components/Button';

interface DropdownButtonProps {
  buttonProps?: React.ComponentProps<typeof Button>;
  items: React.ReactNode[][];
}

export const DropdownButton: React.FC<DropdownButtonProps> = ({
  children,
  buttonProps,
  items,
}) => (
  <DropdownMenuPrimitive.Root>
    <DropdownMenuPrimitive.Trigger>
      <Button {...buttonProps}>{children}</Button>
    </DropdownMenuPrimitive.Trigger>
    <DropdownMenuPrimitive.Portal>
      <DropdownMenuPrimitive.Content align="start">
        <div className="origin-top-left absolute left-0 z-10 mt-xs w-max max-w-xs rounded shadow-md bg-white ring-1 ring-gray-300 divide-y divide-gray-300 focus:outline-none">
          {items.map(group => (
            <div className="py-1">
              {group.map(item => (
                <DropdownMenuPrimitive.Item>
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
