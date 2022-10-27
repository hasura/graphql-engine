import { BrowseRowsContainer } from '@/features/BrowseRows';
import { DatabaseRelationshipsContainer } from '@/features/DataRelationships';
import { getTableName } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Tabs } from '@/new-components/Tabs';
import { getRoute } from '@/features/Data';
import React from 'react';
import { useDispatch } from 'react-redux';
import { useDatabaseHierarchy, useTableDefinition } from '../hooks';
import { ModifyTable } from '../ModifyTable/ModifyTable';
import { Breadcrumbs, TableName } from './parts';
import _push from '../../../components/Services/Data/push';

type AllowedTabs = 'modify' | 'browse' | 'relationship' | 'permissions';
export interface ManageTableProps {
  params: {
    operation: AllowedTabs;
  };
}

const FeatureNotImplemented = () => {
  return (
    <div className="my-4">
      <IndicatorCard headline="Feature is currently unavailable">
        Feature not implemented
      </IndicatorCard>
    </div>
  );
};

const availableTabs = (
  dataSourceName: string,
  table: Table,
  tableName: string
) => [
  {
    value: 'browse',
    label: 'Browse',
    content: (
      <BrowseRowsContainer dataSourceName={dataSourceName} table={table} />
    ),
  },
  {
    value: 'modify',
    label: 'Modify',
    content: (
      <ModifyTable
        dataSourceName={dataSourceName}
        table={table}
        tableName={tableName}
      />
    ),
  },
  {
    value: 'relationships',
    label: 'Relationships',
    content: (
      <DatabaseRelationshipsContainer
        dataSourceName={dataSourceName}
        table={table}
      />
    ),
  },
  {
    value: 'permissions',
    label: 'Permissions',
    content: <FeatureNotImplemented />,
  },
];

export const ManageTable: React.VFC<ManageTableProps> = (
  props: ManageTableProps
) => {
  const {
    params: { operation },
  } = props;

  const urlData = useTableDefinition(window.location);
  const dispatch = useDispatch();

  if (urlData.querystringParseResult === 'error')
    throw Error('Unable to render');

  const { database: dataSourceName, table } = urlData.data;

  const {
    data: databaseHierarchy,
    isLoading,
    isError,
  } = useDatabaseHierarchy(dataSourceName);

  const tableName = databaseHierarchy
    ? getTableName(table, databaseHierarchy)
    : '';

  if (isError)
    return (
      <IndicatorCard status="negative">
        Could not fetch the database hierarchy for the table.
      </IndicatorCard>
    );

  if (isLoading) return <IndicatorCard status="info">Loading...</IndicatorCard>;

  return (
    <div className="w-full overflow-y-auto bg-gray-50">
      <div className="px-md pt-md mb-xs">
        <Breadcrumbs dataSourceName={dataSourceName} tableName={tableName} />
        <TableName dataSourceName={dataSourceName} tableName={tableName} />
        <Tabs
          value={operation}
          onValueChange={_operation => {
            dispatch(
              _push(getRoute().table(dataSourceName, table, _operation))
            );
          }}
          items={availableTabs(dataSourceName, table, tableName)}
        />
      </div>
    </div>
  );
};
