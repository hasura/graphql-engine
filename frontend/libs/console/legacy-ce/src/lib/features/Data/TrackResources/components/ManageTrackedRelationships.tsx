import React from 'react';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { Relationship } from '../../../DatabaseRelationships/types';
import { TrackableResourceTabs } from '../../ManageDatabase/components/TrackableResourceTabs';
import { TrackedRelationshipsContainer } from './TrackedRelationshipsContainer';
import { UntrackedRelationships } from './UntrackedRelationships';

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
    <TrackableResourceTabs
      introText={
        'Create and track a relationship to view it in your GraphQL schema.'
      }
      value={tab}
      data-testid="track-relationships"
      onValueChange={value => setTab(value)}
      isLoading={isLoading}
      items={{
        tracked: {
          amount: trackedFKRelationships.length,
          content: (
            <TrackedRelationshipsContainer dataSourceName={dataSourceName} />
          ),
        },
        untracked: {
          amount: suggestedRelationships.length,
          content: <UntrackedRelationships dataSourceName={dataSourceName} />,
        },
      }}
    />
  );
};
