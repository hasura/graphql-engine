import React from 'react';
import * as RadixTabs from '@radix-ui/react-tabs';
import clsx from 'clsx';

interface TabsItem {
  value: string;
  label: React.ReactNode;
  icon?: React.ReactNode;
  content: React.ReactNode;
}

interface TabsCustomProps extends React.ComponentProps<typeof RadixTabs.Root> {
  headerTabBackgroundColor?: string;
}
interface TabsProps extends TabsCustomProps {
  items: TabsItem[];
}

export const Tabs: React.FC<TabsProps> = props => {
  const { headerTabBackgroundColor, items, ...rest } = props;

  const backgroundColor = headerTabBackgroundColor ?? 'bg-legacybg';
  return (
    <RadixTabs.Root
      defaultValue={rest?.defaultValue ?? items[0]?.value}
      {...rest}
    >
      <RadixTabs.List aria-label="Tabs">
        <div
          className={`border-b border-gray-200 ${backgroundColor} flex space-x-4`}
        >
          {items.map(({ value: itemValue, label, icon }) => (
            <RadixTabs.Trigger key={itemValue} value={itemValue} asChild>
              <button
                className={clsx(
                  'whitespace-nowrap py-xs px-sm border-b-2 font-semibold',
                  'hover:border-gray-300 border-transparent text-muted',
                  'radix-state-active:hover:border-yellow-500 radix-state-active:border-yellow-500 radix-state-active:text-yellow-500',
                  // add focus visible outline for accessibility
                  'focus-visible:outline focus-visible:outline-2 outline-offset-2 focus-visible:outline-secondary'
                )}
              >
                {icon ? (
                  <>
                    {icon} {label}
                  </>
                ) : (
                  label
                )}
              </button>
            </RadixTabs.Trigger>
          ))}
        </div>
      </RadixTabs.List>
      {items.map(({ value: itemValue, content }) => (
        <RadixTabs.Content key={itemValue} value={itemValue}>
          {content}
        </RadixTabs.Content>
      ))}
    </RadixTabs.Root>
  );
};
