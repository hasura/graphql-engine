import React from 'react';
import * as RadixTabs from '@radix-ui/react-tabs';
import clsx from 'clsx';

interface TabsItem {
  value: string;
  label: React.ReactNode;
  icon?: React.ReactNode;
  content: React.ReactNode;
}

export type TabColor =
  | 'green'
  | 'red'
  | 'yellow'
  | 'indigo'
  | 'gray'
  | 'blue'
  | 'purple';

type AccentStyle = 'underline' | 'background';

interface TabsCustomProps extends React.ComponentProps<typeof RadixTabs.Root> {
  headerTabBackgroundColor?: 'white' | 'grey';
}
interface TabsProps extends TabsCustomProps {
  items: TabsItem[];
  color?: TabColor;
  accentStyle?: AccentStyle;
}

const twThemes = {
  underline: {
    yellow: `radix-state-active:border-yellow-500  radix-state-active:text-yellow-500`,
    blue: `radix-state-active:border-blue-500  radix-state-active:text-blue-800`,
    green: `radix-state-active:border-green-500  radix-state-active:text-green-800`,
    red: `radix-state-active:border-red-500  radix-state-active:text-red-800`,
    gray: `radix-state-active:border-gray-500  radix-state-active:text-gray-800`,
    indigo: `radix-state-active:border-indigo-500  radix-state-active:text-indigo-800`,
    purple: `radix-state-active:border-purple-500  radix-state-active:text-purple-800`,
  },
  background: {
    green: `radix-state-active:bg-green-100 radix-state-active:text-green-800`,
    red: `radix-state-active:bg-red-100 radix-state-active:text-red-800`,
    yellow: `radix-state-active:bg-primary-light/80 radix-state-active:!text-slate-700`,
    gray: `radix-state-active:bg-gray-200 radix-state-active:text-gray-900`,
    indigo: `radix-state-active:bg-indigo-100 radix-state-active:text-indigo-800`,
    blue: `radix-state-active:bg-blue-100 radix-state-active:text-blue-800`,
    purple: `radix-state-active:bg-purple-100 radix-state-active:text-purple-800`,
  },
  hoverStyle: {
    yellow: `hover:border-yellow-300`,
    blue: `hover:border-blue-300`,
    green: `hover:border-green-300 `,
    red: `hover:border-red-300 `,
    gray: `hover:border-gray-300`,
    indigo: `hover:border-indigo-300 `,
    purple: `hover:border-purple-300 `,
  },
};

const twTabBackgroundColors = {
  white: `bg-white`,
  grey: `bg-legacybg`,
};

export const Tabs = (props: TabsProps) => {
  const {
    headerTabBackgroundColor = 'grey',
    items,
    color = 'gray',
    accentStyle = 'background',
    ...rest
  } = props;

  return (
    <RadixTabs.Root
      defaultValue={rest?.defaultValue ?? items[0]?.value}
      {...rest}
    >
      <RadixTabs.List aria-label="Tabs">
        <div
          className={`border-b border-gray-200 ${twTabBackgroundColors[headerTabBackgroundColor]} flex space-x-4`}
        >
          {items.map(({ value: itemValue, label, icon }) => (
            <RadixTabs.Trigger key={itemValue} value={itemValue} asChild>
              <button
                className={clsx(
                  'border-transparent rounded-t text-muted whitespace-nowrap py-xs px-sm border-b-2 font-semibold tracking-[.015em] flex items-center gap-2',
                  twThemes.underline[color],
                  twThemes[accentStyle][color],
                  twThemes.hoverStyle[color],
                  // add focus visible outline for accessibility
                  'focus-visible:outline focus-visible:outline-2 outline-offset-4 focus-visible:outline-blue-400'
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
