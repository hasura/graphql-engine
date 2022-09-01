import React from 'react';
import * as RadixTabs from '@radix-ui/react-tabs';
import clsx from 'clsx';

interface TabsItem {
  value: string;
  label: string;
  content: React.ReactNode;
}

interface TabsProps extends React.ComponentProps<typeof RadixTabs.Root> {
  items: TabsItem[];
}

export const Tabs: React.FC<TabsProps> = props => {
  const { items, ...rest } = props;
  return (
    <RadixTabs.Root defaultValue={items[0]?.value} {...rest}>
      <RadixTabs.List aria-label="Tabs">
        <div className="border-b border-gray-200 mb-lg bg-legacybg flex space-x-4">
          {items.map(({ value: itemValue, label }) => (
            <RadixTabs.Trigger value={itemValue} asChild>
              <button
                className={clsx(
                  'whitespace-nowrap py-xs px-sm border-b-2 font-semibold',
                  'hover:border-gray-300 border-transparent text-muted',
                  'radix-state-active:hover:border-yellow-500 radix-state-active:border-yellow-500 radix-state-active:text-yellow-500'
                )}
              >
                {label}
              </button>
            </RadixTabs.Trigger>
          ))}
        </div>
      </RadixTabs.List>
      {items.map(({ value: itemValue, content }) => (
        <RadixTabs.Content value={itemValue}>{content}</RadixTabs.Content>
      ))}
    </RadixTabs.Root>
  );
};
