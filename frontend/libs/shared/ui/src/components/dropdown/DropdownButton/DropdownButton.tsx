import React from 'react';
import { Button } from '../../Button';
import { DropdownMenu, DropdownMenuProps } from '../DropdownMenu';
import { DropdownMenu as ThemeDropdownMenu } from '@radix-ui/themes';

export type DropdownButtonProps = React.ComponentProps<typeof Button> &
  Omit<DropdownMenuProps, 'children'>;

export const DropdownButton: React.FC<DropdownButtonProps> = ({
  items,
  options = {},
  children,
  ...rest
}) => {
  const dropdownMenuOptions =
    // Ensure the disabled state is passed to the trigger. Otherwise, the trigger looks as disabled
    // but it's interactive
    rest?.disabled !== undefined
      ? {
          ...options,
          trigger: {
            ...options.trigger,
            disabled: rest.disabled,
          },
        }
      : options;

  return (
    <DropdownMenu.Root options={dropdownMenuOptions} items={items}>
      <Button
        rightIcon={(props) => <ThemeDropdownMenu.TriggerIcon {...props} />}
        {...rest}
      >
        {children}
      </Button>
    </DropdownMenu.Root>
  );
};
