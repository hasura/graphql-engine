import React from 'react';
import { FaChevronDown } from 'react-icons/fa';
import { Button } from '../Button';
import { DropdownMenu } from '../DropdownMenu';

interface DropdownButtonProps extends React.ComponentProps<typeof Button> {
  items: React.ReactNode[][];
}

export const DropdownButton: React.FC<DropdownButtonProps> = ({
  items,
  ...rest
}) => (
  <DropdownMenu items={items}>
    <Button
      iconPosition="end"
      icon={
        <FaChevronDown className="transition-transform group-radix-state-open:rotate-180 w-3 h-3" />
      }
      size="sm"
      {...rest}
    />
  </DropdownMenu>
);
