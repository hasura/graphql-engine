import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import clsx from 'clsx';
import React from 'react';

export const Separator: React.VFC = () => (
  <DropdownMenu.Separator className={clsx('h-[1px] my-2  bg-gray-300 mx-3')} />
);
