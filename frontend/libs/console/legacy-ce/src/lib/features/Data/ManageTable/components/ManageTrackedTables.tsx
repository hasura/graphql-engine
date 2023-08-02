import React from 'react';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { useMetadata } from '../../../hasura-metadata-api';
import { TrackableResourceTabs } from '../../ManageDatabase/components/TrackableResourceTabs';
import { useIntrospectedTables } from '../../hooks/useIntrospectedTables';
import { TableList } from '../parts/TableList';
import { selectTrackedTables, splitByTracked } from '../selectors';
import { Feature, IntrospectedTable } from '../../../DataSource';
import { useInvalidateSuggestedRelationships } from '../../TrackResources/TrackRelationships/hooks/useSuggestedRelationships';

type TabState = 'tracked' | 'untracked';

export const ManageTrackedTables = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const [tab, setTab] = React.useState<TabState>('tracked');

  const {
    data: metadataTables = [],
    isFetched,
    isError: isMetadataError,
    error: introspectionError,
  } = useMetadata(m => selectTrackedTables(m)(dataSourceName));

  const selector = React.useCallback(
    (introspectedTables: Feature | IntrospectedTable[]) =>
      splitByTracked({ metadataTables, introspectedTables }),
    [metadataTables]
  );

  const {
    data: { trackedTables = [], untrackedTables = [] } = {},
    isSuccess,
    isError: isIntrospectionError,
    error: metadataError,
  } = useIntrospectedTables({
    dataSourceName,
    options: {
      select: selector,
      enabled: isFetched,
      refetchOnWindowFocus: false,
      onSuccess: data => {
        if (data.trackedTables.length === 0) {
          // if user has no tracked tables, switch to the untracked list
          setTab('untracked');
        }
      },
    },
  });
  const { invalidateSuggestedRelationships } =
    useInvalidateSuggestedRelationships({ dataSourceName });

  if (isMetadataError || isIntrospectionError)
    return (
      <IndicatorCard
        status="negative"
        headline="Error while fetching data"
        showIcon
      >
        <div>
          {metadataError?.message ??
            (introspectionError as any)?.message ??
            'Something went wrong'}
        </div>
      </IndicatorCard>
    );

  return (
    <TrackableResourceTabs
      introText={
        'Tracking tables adds them to your GraphQL API. All objects will be admin-only until permissions have been set.'
      }
      value={tab}
      onValueChange={value => {
        setTab(value);
      }}
      isLoading={!isSuccess}
      items={{
        untracked: {
          amount: untrackedTables.length,
          content: (
            <TableList
              viewingTablesThatAre={'untracked'}
              dataSourceName={dataSourceName}
              tables={untrackedTables}
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
              dataSourceName={dataSourceName}
              tables={trackedTables}
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
