import DropdownMenuRoot from './DropdownMenu';
import SubMenu from './DropdownMenu';

import { DropdownMenu as ThemeDropdownMenu } from '@radix-ui/themes';

export type * from './DropdownMenu';

export const DropdownMenu = {
  Root: DropdownMenuRoot,
  Item: ThemeDropdownMenu.Item,
  Separator: ThemeDropdownMenu.Separator,
  CheckboxItem: ThemeDropdownMenu.CheckboxItem,
  Group: ThemeDropdownMenu.Group,
  RadioItem: ThemeDropdownMenu.RadioItem,
  Label: ThemeDropdownMenu.Label,
  RadioGroup: ThemeDropdownMenu.RadioGroup,
  Sub: SubMenu,
} as const;
