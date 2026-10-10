import { DropdownMenu as ThemeDropdownMenu } from '@radix-ui/themes';
import { Fragment } from 'react';

export interface DropdownMenuProps {
  options?: {
    root?: Omit<ThemeDropdownMenu.RootProps, 'children'>;
    trigger?: Omit<ThemeDropdownMenu.TriggerProps, 'children' | 'disabled'>;
    content?: Omit<ThemeDropdownMenu.ContentProps, 'children'>;
  };
  items: React.ReactElement<any>[];
  children: React.ReactNode;
  disabled?: boolean;
}

const DropdownMenuRoot = ({
  children,
  items,
  options,
  disabled,
}: DropdownMenuProps) => (
  <ThemeDropdownMenu.Root {...options?.root}>
    <ThemeDropdownMenu.Trigger disabled={disabled} {...options?.trigger}>
      {children}
    </ThemeDropdownMenu.Trigger>
    <ThemeDropdownMenu.Content {...options?.content}>
      {items.map((item, i) => (
        <Fragment key={i}>{item}</Fragment>
      ))}
    </ThemeDropdownMenu.Content>
  </ThemeDropdownMenu.Root>
);

export default DropdownMenuRoot;
