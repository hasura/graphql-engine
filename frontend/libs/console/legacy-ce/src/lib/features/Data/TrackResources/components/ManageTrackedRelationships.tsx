import { RiInformationFill } from 'react-icons/ri';
import { Collapsible } from '../../../../new-components/Collapsible';
import * as Tabs from '@radix-ui/react-tabs';
import { Tooltip } from '../../../../new-components/Tooltip';
import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { TrackedRelationshipsContainer } from './TrackedRelationshipsContainer';
import { UntrackedRelationships } from './UntrackedRelationships';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { Relationship } from '../../../DatabaseRelationships/types';

const classNames = {
  selected:
    'border-yellow-500 text-yellow-500 whitespace-nowrap p-xs border-b-2 font-semibold -mb-0.5',
  unselected:
    'border-transparent text-muted whitespace-nowrap p-xs border-b-2 font-semibold -mb-0.5 hover:border-gray-300 hover:text-gray-900',
};

type ManageTrackedRelationshipsProps = {
  dataSourceName: string;
  suggestedRelationships: SuggestedRelationshipWithName[];
  trackedFKRelationships: Relationship[];
  isLoading: boolean;
};

export const ManageTrackedRelationships: React.VFC<
  ManageTrackedRelationshipsProps
> = ({
  dataSourceName,
  isLoading,
  suggestedRelationships,
  trackedFKRelationships,
}) => {
  const [tab, setTab] = React.useState<'tracked' | 'untracked'>('untracked');

  if (!suggestedRelationships)
    return <div className="px-md">Something went wrong</div>;

  return (
    <Collapsible
      triggerChildren={
        <div>
          <div className="flex mb-1 items-center">
            <div className="font-semibold inline-flex items-center text-lg">
              Untracked Foreign Key Relationships
            </div>
            <Tooltip
              tooltipContentChildren="Expose the tables available in your database via the GraphQL API"
              side="right"
            >
              <RiInformationFill />
            </Tooltip>
          </div>
        </div>
      }
      // defaultOpen
    >
      <Tabs.Root
        defaultValue="untracked"
        data-testid="track-relationships"
        className="space-y-4"
        onValueChange={value =>
          setTab(value === 'tracked' ? 'tracked' : 'untracked')
        }
      >
        <p className="text-muted">
          Tracking tables adds them to your GraphQL API. All objects will be
          admin-only until permissions have been set.
        </p>

        <Tabs.List
          className="border-b border-gray-300 px-4 flex space-x-4"
          aria-label="Tabs"
        >
          <Tabs.Trigger
            value="untracked"
            className={
              tab === 'untracked' ? classNames.selected : classNames.unselected
            }
          >
            Untracked
            <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
              {suggestedRelationships.length}
            </span>
          </Tabs.Trigger>
          <Tabs.Trigger
            value="tracked"
            className={
              tab === 'tracked' ? classNames.selected : classNames.unselected
            }
          >
            Tracked
            <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
              {trackedFKRelationships.length}
            </span>
          </Tabs.Trigger>
        </Tabs.List>

        {isLoading ? (
          <div className="px-md">
            <Skeleton count={8} height={25} className="mb-2" />
          </div>
        ) : (
          <>
            <Tabs.Content value="tracked" className="px-md">
              <TrackedRelationshipsContainer dataSourceName={dataSourceName} />
            </Tabs.Content>
            <Tabs.Content value="untracked" className="px-md">
              <UntrackedRelationships dataSourceName={dataSourceName} />
            </Tabs.Content>
          </>
        )}
      </Tabs.Root>
    </Collapsible>
  );
};
