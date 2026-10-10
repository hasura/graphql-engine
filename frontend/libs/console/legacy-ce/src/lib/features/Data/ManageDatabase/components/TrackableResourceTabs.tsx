import clsx from 'clsx';
import React from 'react';
import {
  Badge,
  LearnMoreLink,
  SkeletonList,
  Tabs,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

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

  return isLoading ? (
    <div>
      <SkeletonList count={8} containerClassName="mb-2" />
    </div>
  ) : (
    <div data-testid="trackable-resource-tabs" className="mx-sm">
      {introText ? (
        <Flex className="my-4" align="center" gap="2">
          <Text>{introText}</Text>
          {!!learnMoreLink && <LearnMoreLink href={learnMoreLink} />}
        </Flex>
      ) : (
        // spacer:
        <div className="my-4" />
      )}
      <Tabs
        color="gray"
        className={clsx('space-y-4', className)}
        onValueChange={(value) => onValueChange(value as TabState)}
        items={[
          {
            value: 'untracked',
            label: (
              <Flex align="center" gap="2" data-testid="untracked-tab">
                Untracked
                <Badge className={clsx(`px-xs`)} color="gray">
                  {untracked.amount}
                </Badge>
              </Flex>
            ),
            content: untracked.content,
          },
          {
            value: 'tracked',
            label: (
              <Flex align="center" gap="2" data-testid="tracked-tab">
                Tracked
                <Badge className={clsx(`px-xs`)} color="gray">
                  {tracked.amount}
                </Badge>
              </Flex>
            ),
            content: tracked.content,
          },
        ]}
        {...rest}
      />
    </div>
  );
};
