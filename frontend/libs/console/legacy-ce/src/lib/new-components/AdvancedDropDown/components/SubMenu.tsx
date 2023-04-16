import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { BsChevronRight } from 'react-icons/bs';
import { HeightOptions } from './Root';
import * as StyleWrappers from './style-wrappers';

type SubMenuProps = {
  disabled?: boolean;
  label: React.ReactNode;
  maxHeight?: HeightOptions;
};

export const SubMenu: React.FC<SubMenuProps> = ({
  children,
  disabled = false,
  label,
  maxHeight = '65vh',
}) => (
  <DropdownMenu.Sub>
    <DropdownMenu.SubTrigger
      className="group/item outline-none"
      disabled={disabled}
    >
      <StyleWrappers.Item>
        {label}
        <div className="ml-auto pl-5">
          <BsChevronRight />
        </div>
      </StyleWrappers.Item>
    </DropdownMenu.SubTrigger>
    <DropdownMenu.Portal>
      <DropdownMenu.SubContent sideOffset={2} alignOffset={-5}>
        <StyleWrappers.Content maxHeight={maxHeight}>
          {children}
        </StyleWrappers.Content>
      </DropdownMenu.SubContent>
    </DropdownMenu.Portal>
  </DropdownMenu.Sub>
);
