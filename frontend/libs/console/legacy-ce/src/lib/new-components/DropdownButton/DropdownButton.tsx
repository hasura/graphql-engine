import React from 'react';
import produce from 'immer';
import { FaChevronDown } from 'react-icons/fa';
import { Button } from '../Button';
import { DropdownMenu, DropdownMenuProps } from '../DropdownMenu';

interface DropdownButtonProps extends React.ComponentProps<typeof Button> {
  items: React.ReactNode[][];
  options?: DropdownMenuProps['options'];
  size?: 'sm' | 'md' | 'lg';
}

export const DropdownButton: React.FC<DropdownButtonProps> = ({
  items,
  options = {},
  size,
  ...rest
}) => {
  const dropdownMenuOptions = produce(options, draft => {
    // Ensure the disabled state is passed to the trigger. Otherwise, the trigger looks as disabled
    // but it's interactive
    if (rest?.disabled !== undefined) {
      draft.trigger ??= {};
      draft.trigger.disabled = rest.disabled;
    }
  });

  return (
    <DropdownMenu options={dropdownMenuOptions} items={items}>
      <Button
        iconPosition="end"
        icon={
          <FaChevronDown className="transition-transform group-radix-state-open:rotate-180 w-3 h-3" />
        }
        size={size ?? 'sm'}
        {...rest}
      />
    </DropdownMenu>
  );
};
