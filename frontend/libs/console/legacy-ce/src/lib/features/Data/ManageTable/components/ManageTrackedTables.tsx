import React from 'react';
import { Feature, IntrospectedTable } from '../../../DataSource';
import { MetadataUtils, useMetadata } from '../../../hasura-metadata-api';
import { supportsSchemaLessTables } from '../../LogicalModels/LogicalModelWidget/utils';
import { TrackableResourceTabs } from '../../ManageDatabase/components/TrackableResourceTabs';
import { useInvalidateSuggestedRelationships } from '../../TrackResources/TrackRelationships/hooks/useSuggestedRelationships';
import { ReactQueryStatusUI } from '../../components';
import { multipleQueryUtils } from '../../components/ReactQueryWrappers/utils';
import { useDriverCapabilities } from '../../hooks/useDriverCapabilities';
import { useIntrospectedTables } from '../../hooks/useIntrospectedTables';
import { TableList } from '../parts/TableList';
import {
  PayloadTable,
  selectTrackedTables,
  splitByTracked,
} from '../selectors';

type TabState = 'tracked' | 'untracked';

const DataBound = ({ dataSourceName }: { dataSourceName: string }) => {
  // we need both the tables and the source
  const metadataResult = useMetadata(m => ({
    metadataTables: selectTrackedTables(m)(dataSourceName),
    dataSource: MetadataUtils.findMetadataSource(dataSourceName, m),
  }));

  const {
    data: { metadataTables = [], dataSource } = {},
    status: metadataStatus,
  } = metadataResult;

  const introspectSelector = React.useCallback(
    (introspectedTables: Feature | IntrospectedTable[]) =>
      splitByTracked({ metadataTables, introspectedTables }),
    [metadataTables]
  );

  const introspectedTablesResult = useIntrospectedTables({
    dataSourceName,
    options: {
      select: introspectSelector,
      enabled: metadataStatus === 'success',
      refetchOnWindowFocus: false,
    },
  });

  const { data: capabilities } = useDriverCapabilities({
    dataSourceName: dataSourceName,
  });

  const areSchemaLessTablesSupported = supportsSchemaLessTables(capabilities);

  const tablesLabel = dataSource?.kind === 'mongo' ? 'collections' : 'tables';

  if (!introspectedTablesResult.isSuccess || !metadataResult.isSuccess) {
    return (
      <ReactQueryStatusUI
        status={multipleQueryUtils.status([
          metadataResult,
          introspectedTablesResult,
        ])}
        error={multipleQueryUtils.firstError([
          metadataResult,
          introspectedTablesResult,
        ])}
      />
    );
  }

  return (
    <ManageTrackedTablesUI
      dataSourceName={dataSourceName}
      tablesLabel={tablesLabel}
      trackMultipleEnabled={!areSchemaLessTablesSupported}
      {...introspectedTablesResult.data}
    />
  );
};

const ManageTrackedTablesUI = ({
  dataSourceName,
  trackedTables,
  untrackedTables,
  tablesLabel = 'tables',
  trackMultipleEnabled = true,
  untrackMultipleEnabled = true,
}: {
  dataSourceName: string;
  untrackedTables: PayloadTable[];
  trackedTables: PayloadTable[];
  tablesLabel?: string;
  trackMultipleEnabled?: boolean;
  untrackMultipleEnabled?: boolean;
}) => {
  const [tab, setTab] = React.useState<TabState>(
    trackedTables.length === 0 ? 'untracked' : 'tracked'
  );
  const { invalidateSuggestedRelationships } =
    useInvalidateSuggestedRelationships({ dataSourceName });

  return (
    <TrackableResourceTabs
      introText={`Tracking ${tablesLabel} adds them to your GraphQL API. All objects will be admin-only until permissions have been set.`}
      value={tab}
      onValueChange={value => {
        setTab(value);
      }}
      items={{
        untracked: {
          amount: untrackedTables.length,
          content: (
            <TableList
              viewingTablesThatAre={'untracked'}
              dataSourceName={dataSourceName}
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
              dataSourceName={dataSourceName}
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
