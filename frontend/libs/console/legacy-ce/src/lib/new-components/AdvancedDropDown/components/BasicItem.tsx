import React from 'react';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import * as StyleWrappers from './style-wrappers';

export const BasicItem: React.FC<{
  disabled?: boolean;
  onClick?: () => void;
  dangerous?: boolean;
  link?: boolean;
  query?: string;
}> = ({ disabled, onClick, children, dangerous, link, query }) => {
  if (typeof children === 'string' && !!query && !children.includes(query)) {
    return null;
  }
  return (
    <DropdownMenu.Item
      disabled={disabled}
      className="group/item outline-none"
      onClick={onClick}
    >
      <StyleWrappers.Item dangerous={dangerous} link={link} disabled={disabled}>
        {children}
      </StyleWrappers.Item>
    </DropdownMenu.Item>
  );
};
