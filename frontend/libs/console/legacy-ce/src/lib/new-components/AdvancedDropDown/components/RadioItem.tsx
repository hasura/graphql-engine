import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { BsDot } from 'react-icons/bs';
import * as StyleWrappers from './style-wrappers';

export const RadioItem: React.FC<{ value: string }> = ({ value, children }) => (
  <DropdownMenu.RadioItem className="group/item outline-none" value={value}>
    {console.log(value)}
    <StyleWrappers.Item selectable>
      <DropdownMenu.ItemIndicator className="absolute left-0 w-7 inline-flex items-center justify-center">
        <BsDot size={24} className="text-blue-600" />
      </DropdownMenu.ItemIndicator>
      {children}
    </StyleWrappers.Item>
  </DropdownMenu.RadioItem>
);
