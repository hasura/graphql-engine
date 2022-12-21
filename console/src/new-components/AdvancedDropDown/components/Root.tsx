import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';

import * as StyleWrappers from './style-wrappers';

export const Root: React.FC<{
  trigger: React.ReactNode;

  defaultOpen?: boolean;
}> = ({ children, trigger, defaultOpen = false }) => (
  <DropdownMenu.Root defaultOpen={defaultOpen}>
    <DropdownMenu.Trigger>{trigger}</DropdownMenu.Trigger>
    <DropdownMenu.Portal>
      <DropdownMenu.Content className="group/content" sideOffset={5}>
        <StyleWrappers.Content>{children}</StyleWrappers.Content>
        <DropdownMenu.Arrow className="fill-white" />
      </DropdownMenu.Content>
    </DropdownMenu.Portal>
  </DropdownMenu.Root>
);
