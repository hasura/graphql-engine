import React from 'react';
import { Tabs } from '../../../../new-components/Tabs';
import clsx from 'clsx';
import Skeleton from 'react-loading-skeleton';

export type TabState = 'tracked' | 'untracked';

type ManageResourceTabsProps = Omit<
  React.ComponentProps<typeof Tabs>,
  'items' | 'onValueChange' | 'value'
> & {
  items: {
    tracked: { amount: number; content: React.ReactNode };
    untracked: { amount: number; content: React.ReactNode };
  };
  onValueChange: (value: TabState) => void;
  value: TabState;
  introText?: string;
  isLoading?: boolean;
};

/**
 *
 * This is a wrapper around the `<Tabs />` component that simplifies and specializes the props API to be used to display tabbed lists of Trackable Resources
 *
 */
export const TrackableResourceTabs = ({
  items,
  onValueChange,
  className,
  introText,
  isLoading,
  ...rest
}: ManageResourceTabsProps) => {
  const { untracked, tracked } = items;

  return isLoading ? (
    <div className="mx-sm">
      <Skeleton count={8} height={25} className="mb-2" />
    </div>
  ) : (
    <div data-testid="trackable-resource-tabs" className="mx-sm min-h-[20rem]">
      {!!introText && <p className="text-muted my-2">{introText}</p>}
      <Tabs
        className={clsx('space-y-4', className)}
        onValueChange={value => onValueChange(value as TabState)}
        items={[
          {
            value: 'untracked',
            label: (
              <div className="flex items-center" data-testid="untracked-tab">
                Untracked
                <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
                  {untracked.amount}
                </span>
              </div>
            ),
            content: untracked.content,
          },
          {
            value: 'tracked',
            label: (
              <div className="flex items-center" data-testid="tracked-tab">
                Tracked
                <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
                  {tracked.amount}
                </span>
              </div>
            ),
            content: tracked.content,
          },
        ]}
        {...rest}
      />
    </div>
  );
};
