import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import React from 'react';
import * as StyleWrappers from './style-wrappers';

export const Label: React.FC = ({ children }) => (
  <DropdownMenu.Label>
    <StyleWrappers.Label>{children}</StyleWrappers.Label>
  </DropdownMenu.Label>
);
