import React from 'react';
import { TrackableResourceTabs } from '../../../../features/Data/ManageDatabase/components';
import { CollapsibleResource } from '../../../../features/Data/ManageDatabase/parts';
import { TableList } from '../../../../features/Data/ManageTable/parts/TableList';
import { PostgresTable } from '../../../../features/DataSource';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '../../../../features/FeatureFlags';
import { exportMetadata } from '../../../../metadata/actions';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import {
  getSchemaBaseRoute,
  getTableModifyRoute,
} from '../../../Common/utils/routesUtils';
import { updateSchemaInfo } from '../DataActions';
import _push from '../push';
import { ExperimentalFeatureBanner } from '../../../../features/components';
import { useTrackTablesState } from './useTrackTablesState';

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

  const onMultiple = React.useCallback(() => {
    dispatch(exportMetadata()).then(
      dispatch(updateSchemaInfo()).then(() => {
        dispatch(_push(getSchemaBaseRoute(schema, dataSourceName)));
      })
    );
  }, [dataSourceName, dispatch, schema]);

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
    <CollapsibleResource
      title={<div>Untracked tables or views</div>}
      tooltip="Tables or views that are not exposed over the GraphQL API"
      defaultOpen
      key={`${dataSourceName}-${schema}`}
    >
      <ExperimentalFeatureBanner
        githubIssueLink={
          'https://github.com/hasura/graphql-engine/discussions/9727'
        }
      />
      <div className="my-2 text-muted">
        Tracking tables adds them to your GraphQL API. All objects will be
        admin-only until permissions have been set.
      </div>
      <TrackableResourceTabs
        value={tab}
        onValueChange={value => {
          setTab(value);
        }}
        isLoading={isMetaLoading || isIntroLoading}
        items={{
          untracked: {
            amount: untrackedTables.length,
            content: (
              <TableList
                viewingTablesThatAre={'untracked'}
                dataSourceName={dataSourceName}
                tables={untrackedTables}
                onMultipleTablesTrack={onMultiple}
                onSingleTableTrack={table => {
                  dispatch(
                    _push(
                      getTableModifyRoute(
                        schema,
                        dataSourceName,
                        (table.table as PostgresTable).name,
                        table.type === 'BASE TABLE'
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
                onMultipleTablesTrack={onMultiple}
                onSingleTableTrack={table => {
                  onMultiple();
                }}
              />
            ),
          },
        }}
      />
    </CollapsibleResource>
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
