import clsx from 'clsx';
import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { Badge } from '../../../../new-components/Badge';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { Tabs } from '../../../../new-components/Tabs';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '../../../FeatureFlags';
import { TAB_COLORS } from '../constants';

export type TabState = 'tracked' | 'untracked';

type ManageResourceTabsProps = Omit<
  React.ComponentProps<typeof Tabs>,
  'items' | 'onValueChange'
> & {
  items: {
    tracked: { amount: number; content: React.ReactNode };
    untracked: { amount: number; content: React.ReactNode };
  };
  onValueChange: (value: TabState) => void;
  introText?: string;
  isLoading?: boolean;
  learnMoreLink?: string;
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
  learnMoreLink,
  ...rest
}: ManageResourceTabsProps) => {
  const { untracked, tracked } = items;

  const { enabled: newTabbedUIEnabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.manageDatabaseTabbedInterface
  );

  return isLoading ? (
    <div className="mx-sm">
      <Skeleton count={8} height={25} className="mb-2" />
    </div>
  ) : (
    <div data-testid="trackable-resource-tabs" className="mx-sm">
      {introText ? (
        <div className="my-4 text-muted">
          {introText}
          {!!learnMoreLink && <LearnMoreLink href={learnMoreLink} />}
        </div>
      ) : (
        // spacer:
        <div className="my-4" />
      )}
      <Tabs
        color={newTabbedUIEnabled ? TAB_COLORS.trackingLevel : 'yellow'}
        accentStyle={newTabbedUIEnabled ? 'background' : 'underline'}
        className={clsx('space-y-4', className)}
        onValueChange={value => onValueChange(value as TabState)}
        items={[
          {
            value: 'untracked',
            label: (
              <div
                className="flex items-center gap-2"
                data-testid="untracked-tab"
              >
                Untracked
                <Badge className="px-xs" color="gray">
                  {untracked.amount}
                </Badge>
              </div>
            ),
            content: untracked.content,
          },
          {
            value: 'tracked',
            label: (
              <div
                className="flex items-center gap-2"
                data-testid="tracked-tab"
              >
                Tracked
                <Badge className="px-xs" color="gray">
                  {tracked.amount}
                </Badge>
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
