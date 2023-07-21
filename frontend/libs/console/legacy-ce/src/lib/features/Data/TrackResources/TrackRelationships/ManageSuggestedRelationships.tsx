import React from 'react';
import { ManageDatabaseProps } from '../../ManageDatabase/ManageDatabase';
import { TrackableResourceTabs } from '../../ManageDatabase/components';
import { TabState } from '../../ManageDatabase/components/TrackableResourceTabs';
import { TrackedSuggestedRelationships } from './components/TrackedRelationships';
import { UntrackedRelationships } from './components/UntrackedRelationships';
import { useSuggestedRelationships } from './hooks/useSuggestedRelationships';

export const ManageSuggestedRelationships = ({
  dataSourceName,
  schema,
}: ManageDatabaseProps) => {
  const [tab, setTab] = React.useState<TabState>('untracked');

  const {
    data: { tracked = [], untracked = [] } = {},
    isLoading,
    invalidateQuery,
  } = useSuggestedRelationships({
    dataSourceName,
    which: 'all',
    schema,
  });

  return (
    <TrackableResourceTabs
      isLoading={isLoading}
      introText="Tracking relationships adds them to your API as GraphQL schema relationships"
      learnMoreLink={
        'https://hasura.io/docs/latest/schema/postgres/table-relationships/index/#table-relationships'
      }
      items={{
        tracked: {
          amount: tracked.length,
          content: (
            <TrackedSuggestedRelationships
              dataSourceName={dataSourceName}
              trackedRelationships={tracked}
              onChange={() => {
                invalidateQuery();
              }}
            />
          ),
        },
        untracked: {
          amount: untracked.length,
          content: (
            <UntrackedRelationships
              untrackedRelationships={untracked}
              dataSourceName={dataSourceName}
              onTrack={() => {
                invalidateQuery();
              }}
            />
          ),
        },
      }}
      value={tab}
      onValueChange={setTab}
    />
  );
};
