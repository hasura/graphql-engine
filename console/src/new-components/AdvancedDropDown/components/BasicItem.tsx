import React from 'react';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import * as StyleWrappers from './style-wrappers';

export const BasicItem: React.FC<{
  disabled?: boolean;
  onClick?: () => void;
  dangerous?: boolean;
}> = ({ disabled, onClick, children, dangerous }) => (
  <DropdownMenu.Item
    disabled={disabled}
    className="group/item outline-none"
    onClick={onClick}
  >
    <StyleWrappers.Item dangerous={dangerous}>{children}</StyleWrappers.Item>
  </DropdownMenu.Item>
);
