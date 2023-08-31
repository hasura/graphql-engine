import React from 'react';
import { TrackableResourceTabs } from '../../../../features/Data/ManageDatabase/components';
import { CollapsibleResource } from '../../../../features/Data/ManageDatabase/parts';
import { TableList } from '../../../../features/Data/ManageTable/parts/TableList';
import { ManageSuggestedRelationships } from '../../../../features/Data/TrackResources/TrackRelationships/ManageSuggestedRelationships';
import { useInvalidateSuggestedRelationships } from '../../../../features/Data/TrackResources/TrackRelationships/hooks/useSuggestedRelationships';
import { PostgresTable } from '../../../../features/DataSource';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '../../../../features/FeatureFlags';
import { ExperimentalFeatureBanner } from '../../../../features/components';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { getTableModifyRoute } from '../../../Common/utils/routesUtils';
import { updateSchemaInfo } from '../DataActions';
import _push from '../push';
import { useTrackTablesState } from './useTrackTablesState';
import { ManageTrackedFunctions } from '../../../../features/Data/TrackResources/TrackFunctions/components/ManageTrackedFunctions';
export const TrackTablesContainer = ({
  dataSourceName,
  schema,
  dispatch,
}: {
  dataSourceName: string;
  schema: string;
  dispatch: any;
}) => {
  const {
    trackedTables,
    untrackedTables,
    tab,
    setTab,
    introspectionError,
    isIntroLoading,
    isIntrospectionError,
    isMetaLoading,
    isMetadataError,
    metadataError,
  } = useTrackTablesState(dataSourceName, schema);

  const { invalidateSuggestedRelationships } =
    useInvalidateSuggestedRelationships({ dataSourceName });

  const onChange = React.useCallback(() => {
    invalidateSuggestedRelationships();
    dispatch(updateSchemaInfo());
  }, [dispatch, invalidateSuggestedRelationships]);

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
    <div>
      <CollapsibleResource
        title={<div>Untracked tables or views</div>}
        tooltip="Tables or views that are not exposed over the GraphQL API"
        defaultOpen
        key={`tables-${dataSourceName}-${schema}`}
      >
        <ExperimentalFeatureBanner
          githubIssueLink={
            'https://github.com/hasura/graphql-engine/discussions/9727'
          }
        />
        <TrackableResourceTabs
          value={tab}
          onValueChange={value => {
            setTab(value);
          }}
          introText="Tracking tables adds them to your GraphQL API. All objects will be
          admin-only until permissions have been set."
          learnMoreLink={
            'https://hasura.io/docs/latest/schema/postgres/tables/#tracking-tables'
          }
          isLoading={isMetaLoading || isIntroLoading}
          items={{
            untracked: {
              amount: untrackedTables.length,
              content: (
                <TableList
                  viewingTablesThatAre={'untracked'}
                  dataSourceName={dataSourceName}
                  tables={untrackedTables}
                  onChange={onChange}
                  trackMultipleEnabled
                  onSingleTableTrack={table => {
                    dispatch(
                      _push(
                        getTableModifyRoute(
                          schema,
                          dataSourceName,
                          (table.table as PostgresTable).name,
                          table.type !== 'VIEW'
                        )
                      )
                    );
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
                  onChange={onChange}
                  trackMultipleEnabled
                />
              ),
            },
          }}
        />
      </CollapsibleResource>
      <CollapsibleResource
        title={<div>Untracked Foreign-key relationships</div>}
        tooltip="Tables or views that are not exposed over the GraphQL API"
        defaultOpen
        key={`relationships-${dataSourceName}-${schema}`}
      >
        <ExperimentalFeatureBanner
          githubIssueLink={
            'https://github.com/hasura/graphql-engine/discussions/9727'
          }
        />
        <ManageSuggestedRelationships
          dataSourceName={dataSourceName}
          schema={schema}
        />
      </CollapsibleResource>
      <CollapsibleResource
        title={<div>Untracked custom functions</div>}
        tooltip="Custom functions that are not exposed over the GraphQL APICustom"
        defaultOpen
        key={`${dataSourceName}-${schema}`}
      >
        <ExperimentalFeatureBanner
          githubIssueLink={
            'https://github.com/hasura/graphql-engine/discussions/9727'
          }
        />
        <ManageTrackedFunctions
          dataSourceName={dataSourceName}
          schema={schema}
        />
      </CollapsibleResource>
    </div>
  );
};

export const FeatureFlagContainer = ({
  children,
  ...props
}: {
  dataSourceName: string;
  schema: string;
  dispatch: any;
  children: React.ReactNode;
}) => {
  const { enabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.trackingSectionUI
  );

  if (enabled) return <TrackTablesContainer {...props} />;

  return children;
};
