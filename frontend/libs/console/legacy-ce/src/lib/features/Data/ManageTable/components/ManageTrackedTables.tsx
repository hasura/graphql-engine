import React from 'react';
import {
  useDriverCapabilities,
  useTrackedAndUntrackedTables,
  supportsSchemaLessTables,
} from '@hasura/metadata/data-source';
import {
  TrackableTable,
  useInvalidateSuggestedRelationships,
} from '@hasura/metadata/api';
import { TrackableResourceTabs } from '../../ManageDatabase/components/TrackableResourceTabs';
import { TableList } from '../parts/TableList';
import { IndicatorCard, SkeletonList } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';
import { QualifiedDataSource, Source } from '@hasura/shared/types';

type TabState = 'tracked' | 'untracked';

const DataBound = ({ source }: { source: Source }) => {
  // we need both the tables and the source
  const { introspectionError, isIntroLoading, trackedTables, untrackedTables } =
    useTrackedAndUntrackedTables({
      source,
    });

  const { data: capabilities } = useDriverCapabilities({
    source,
  });

  if (isIntroLoading) {
    return <SkeletonList count={5} />;
  }

  if (introspectionError || !source) {
    return (
      <IndicatorCard
        status="negative"
        headline="Failed to fetch trackable tables"
        showIcon
      >
        {getErrorMessage(introspectionError)}
      </IndicatorCard>
    );
  }

  const areSchemaLessTablesSupported = supportsSchemaLessTables(capabilities);

  const tablesLabel = source.kind === 'mongodb' ? 'collections' : 'tables';

  return (
    <ManageTrackedTablesUI
      source={source}
      tablesLabel={tablesLabel}
      trackMultipleEnabled={!areSchemaLessTablesSupported}
      trackedTables={trackedTables}
      untrackedTables={untrackedTables}
    />
  );
};

const ManageTrackedTablesUI = ({
  source,
  trackedTables,
  untrackedTables,
  tablesLabel = 'tables',
  trackMultipleEnabled = true,
  untrackMultipleEnabled = true,
}: {
  source: QualifiedDataSource;
  untrackedTables: TrackableTable[];
  trackedTables: TrackableTable[];
  tablesLabel?: string;
  trackMultipleEnabled?: boolean;
  untrackMultipleEnabled?: boolean;
}) => {
  const [tab, setTab] = React.useState<TabState>(
    trackedTables.length === 0 ? 'untracked' : 'tracked',
  );
  const invalidateSuggestedRelationships = useInvalidateSuggestedRelationships({
    dataSourceName: source.name,
  });

  return (
    <TrackableResourceTabs
      introText={`Tracking ${tablesLabel} adds them to your GraphQL API. All objects will be admin-only until permissions have been set.`}
      value={tab}
      onValueChange={(value) => {
        setTab(value);
      }}
      items={{
        untracked: {
          amount: untrackedTables.length,
          content: (
            <TableList
              viewingTablesThatAre={'untracked'}
              source={source}
              tables={untrackedTables}
              trackMultipleEnabled={trackMultipleEnabled}
              onChange={() => {
                invalidateSuggestedRelationships();
              }}
            />
          ),
        },
        tracked: {
          amount: trackedTables.length,
          content: (
            <TableList
              viewingTablesThatAre={'tracked'}
              source={source}
              tables={trackedTables}
              trackMultipleEnabled={untrackMultipleEnabled}
              onChange={() => {
                invalidateSuggestedRelationships();
              }}
            />
          ),
        },
      }}
    />
  );
};

export const ManageTrackedTables = DataBound;
