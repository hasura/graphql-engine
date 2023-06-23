import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { FaCheckSquare, FaRegSquare } from 'react-icons/fa';
import * as StyleWrappers from './style-wrappers';

export const CheckItem: React.FC<{
  checked: boolean;
  onCheckChange?: (checked: boolean) => void;
}> = ({ checked, onCheckChange, children }) => (
  <DropdownMenu.CheckboxItem
    className="group/item outline-none"
    checked={checked}
    onClick={e => {
      e.preventDefault();
      onCheckChange?.(!checked);
    }}
  >
    <StyleWrappers.Item selectable>
      <div className="absolute left-0 w-7 inline-flex items-center justify-center">
        {!checked ? (
          <FaRegSquare className="text-blue-600" />
        ) : (
          <FaCheckSquare className="text-blue-500" />
        )}
      </div>
      {children}
    </StyleWrappers.Item>
  </DropdownMenu.CheckboxItem>
);
