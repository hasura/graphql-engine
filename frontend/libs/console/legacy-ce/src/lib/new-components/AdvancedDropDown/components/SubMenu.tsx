import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { BsChevronRight } from 'react-icons/bs';
import * as StyleWrappers from './style-wrappers';

type SubMenuProps = {
  disabled?: boolean;
  label: string;
};

export const SubMenu: React.FC<SubMenuProps> = ({
  children,
  disabled = false,
  label,
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
        <StyleWrappers.Content>{children}</StyleWrappers.Content>
      </DropdownMenu.SubContent>
    </DropdownMenu.Portal>
  </DropdownMenu.Sub>
);
