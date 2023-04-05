import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { Nullable } from '../../../types';

import * as StyleWrappers from './style-wrappers';

type RootProps = {
  trigger: React.ReactNode;
  defaultOpen?: boolean;
  align?: DropdownMenu.DropdownMenuContentProps['align'];
  side?: DropdownMenu.DropdownMenuContentProps['side'];
  arrow?: boolean;
  container?: Nullable<HTMLElement>;
};

export const Root: React.FC<RootProps> = ({
  children,
  trigger,
  defaultOpen = false,
  align,
  side,
  arrow = true,
  container,
}) => (
  <DropdownMenu.Root defaultOpen={defaultOpen}>
    <DropdownMenu.Trigger>{trigger}</DropdownMenu.Trigger>
    <DropdownMenu.Portal container={container}>
      <DropdownMenu.Content
        className="group/content"
        sideOffset={5}
        align={align}
        side={side}
      >
        <StyleWrappers.Content>{children}</StyleWrappers.Content>
        {arrow && <DropdownMenu.Arrow className="fill-white" />}
      </DropdownMenu.Content>
    </DropdownMenu.Portal>
  </DropdownMenu.Root>
);
