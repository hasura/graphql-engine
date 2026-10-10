import React from 'react';
import { Flex, Tabs as ThemeTabs } from '@radix-ui/themes';

export interface TabsItem {
  value: string;
  label: React.ReactNode;
  icon?: React.ReactNode;
  content?: React.ReactNode;
}

export type TabsProps = ThemeTabs.ListProps &
  Pick<ThemeTabs.RootProps, 'value' | 'defaultValue' | 'onValueChange'> & {
    items: TabsItem[];
  };

export const Tabs = (props: TabsProps) => {
  const { className, items, defaultValue, value, onValueChange, ...rest } =
    props;

  return (
    <ThemeTabs.Root
      className={className}
      value={value}
      defaultValue={defaultValue ?? items[0]?.value}
      onValueChange={onValueChange}
    >
      <ThemeTabs.List {...rest}>
        {items.map(({ value: itemValue, label, icon }) => (
          <ThemeTabs.Trigger key={`tab-${itemValue}`} value={itemValue}>
            <Flex gap="2" align="center">
              {icon}
              {label}
            </Flex>
          </ThemeTabs.Trigger>
        ))}
      </ThemeTabs.List>
      {items.map(({ value: itemValue, content }) =>
        content ? (
          <ThemeTabs.Content key={`tab-content-${itemValue}`} value={itemValue}>
            {content}
          </ThemeTabs.Content>
        ) : null,
      )}
    </ThemeTabs.Root>
  );
};
