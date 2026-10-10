import React from 'react';
import { TrackableResourceTabs } from '../../ManageDatabase/components';
import { TabState } from '../../ManageDatabase/components/TrackableResourceTabs';
import { TrackedSuggestedRelationships } from './components/TrackedRelationships';
import { UntrackedRelationships } from './components/UntrackedRelationships';
import { ReactQueryUIWrapper } from '../../components';
import {
  useInvalidateSuggestedRelationships,
  useSuggestedRelationships,
} from '@hasura/metadata/api';
import { Source } from '@hasura/shared/types';

export const ManageSuggestedRelationships = ({
  source,
  schema,
}: {
  source: Source;
  schema?: string;
}) => {
  const [tab, setTab] = React.useState<TabState>('untracked');

  const invalidateQuery = useInvalidateSuggestedRelationships({
    dataSourceName: source.name,
  });
  const suggestedRelationshipsResult = useSuggestedRelationships({
    dataSourceName: source.name,
    which: 'all',
    schema,
  });

  return (
    <ReactQueryUIWrapper
      useQueryResult={suggestedRelationshipsResult}
      render={({ data: { tracked = [], untracked = [] } }) => (
        <TrackableResourceTabs
          introText="Tracking relationships adds them to your API as GraphQL schema relationships"
          learnMoreLink={
            'https://hasura.io/docs/latest/schema/postgres/table-relationships/index/#table-relationships'
          }
          items={{
            tracked: {
              amount: tracked.length,
              content: (
                <TrackedSuggestedRelationships
                  dataSourceName={source.name}
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
                  dataSourceName={source.name}
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
      )}
    />
  );
};
