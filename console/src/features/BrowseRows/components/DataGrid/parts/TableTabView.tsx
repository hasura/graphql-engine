import React from 'react';
import * as RadixTabs from '@radix-ui/react-tabs';
import clsx from 'clsx';
import { FaLink, FaRegTimesCircle, FaTimes } from 'react-icons/fa';

interface TabsItem {
  value: string;
  label: string;
  content: React.ReactNode;
}

interface TableTabViewProps
  extends React.ComponentProps<typeof RadixTabs.Root> {
  items: TabsItem[];
  activeTab: string;
  onTabClick: (value: string) => void;
  onTabClose: (value: string) => void;
  onCloseAll: () => void;
}

export const TableTabView: React.FC<TableTabViewProps> = props => {
  const { items, activeTab, onTabClick, onTabClose, onCloseAll, ...rest } =
    props;
  return (
    <RadixTabs.Root defaultValue={items[0]?.value} value={activeTab} {...rest}>
      <div className="flex justify-between">
        <RadixTabs.List aria-label="Tabs" className="overflow-x-auto">
          <div className="border-gray-200 bg-gray-200 flex space-x-1 w-fit rounded-t">
            {items.map(({ value: itemValue, label }, index) => (
              <RadixTabs.Trigger key={label} value={itemValue} asChild>
                <div
                  className={clsx(
                    'flex whitespace-nowrap py-xs px-sm text-muted',
                    itemValue === activeTab
                      ? 'font-semibold bg-gray-50 from-transparent to-white border-gray-300 border border-b-0 rounded-t'
                      : 'bg-gray-200 text-muted',
                    'radix-state-active:font-bold radix-state-active:bg-white'
                  )}
                  id="container"
                >
                  <div className={clsx('whitespace-nowrap px-sm')}>
                    <button
                      onClick={() => onTabClick(itemValue)}
                      data-testid={`@tab-${label}`}
                    >
                      {index !== 0 && <FaLink className="mr-0.5" />}
                      {label}
                    </button>
                  </div>

                  {index !== 0 && (
                    <div>
                      <button
                        onClick={() => onTabClose(itemValue)}
                        data-testid={`@tab-${label}-close`}
                      >
                        <FaTimes />
                      </button>
                    </div>
                  )}
                </div>
              </RadixTabs.Trigger>
            ))}
          </div>
        </RadixTabs.List>
        {items.length > 1 && (
          <div className="bg-white border border-b-0 border-gray-300 flex items-center px-sm rounded-t to-white whitespace-nowrap">
            <button
              className="flex items-center gap-2 text-red-600"
              onClick={onCloseAll}
            >
              Close All <FaRegTimesCircle />
            </button>
          </div>
        )}
      </div>

      {items.map(({ value: itemValue, content }) => (
        <RadixTabs.Content key={itemValue} value={itemValue}>
          {content}
        </RadixTabs.Content>
      ))}
    </RadixTabs.Root>
  );
};
