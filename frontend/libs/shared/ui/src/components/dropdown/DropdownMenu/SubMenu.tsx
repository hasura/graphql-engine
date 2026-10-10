import { DropdownMenu } from '@radix-ui/themes';
import React, { forwardRef } from 'react';

export type DropdownSubMenuProps = {
  options?: {
    root?: Omit<DropdownMenu.SubProps, 'children'>;
    trigger?: Omit<
      DropdownMenu.SubTriggerProps,
      'children' | 'disabled' | 'ref'
    >;
    content?: Omit<DropdownMenu.SubContentProps, 'children'>;
  };
  items: React.ReactElement<any>[];
  children: React.ReactNode;
  disabled?: boolean;
};

const SubMenu = forwardRef<HTMLDivElement, DropdownSubMenuProps>(
  ({ children, disabled = false, items, options }, ref) => (
    <DropdownMenu.Sub {...options?.root}>
      <DropdownMenu.SubTrigger
        {...options?.trigger}
        ref={ref}
        disabled={disabled}
      >
        {children}
      </DropdownMenu.SubTrigger>
      <DropdownMenu.SubContent {...options?.content}>
        {items}
      </DropdownMenu.SubContent>
    </DropdownMenu.Sub>
  ),
);

export default SubMenu;
