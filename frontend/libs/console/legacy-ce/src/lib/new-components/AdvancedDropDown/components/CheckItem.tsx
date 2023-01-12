import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { BsCheck } from 'react-icons/bs';
import * as StyleWrappers from './style-wrappers';

export const CheckItem: React.FC<{
  checked: boolean;
  onCheckChange?: (checked: boolean) => void;
}> = ({ checked, onCheckChange, children }) => (
  <DropdownMenu.CheckboxItem
    className="group/item outline-none"
    checked={checked}
    onCheckedChange={onCheckChange}
  >
    <StyleWrappers.Item selectable>
      <DropdownMenu.ItemIndicator className="absolute left-0 w-7 inline-flex items-center justify-center">
        <BsCheck size={15} />
      </DropdownMenu.ItemIndicator>
      {children}
    </StyleWrappers.Item>
  </DropdownMenu.CheckboxItem>
);
