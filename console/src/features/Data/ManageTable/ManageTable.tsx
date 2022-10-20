import { BrowseRowsContainer } from '@/features/BrowseRows';
import { getTableName } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Tabs } from '@/new-components/Tabs';
import React, { useState } from 'react';
import { useDatabaseHierarchy } from '../hooks';
import { ModifyTable } from '../ModifyTable/ModifyTable';
import { Breadcrumbs, TableName } from './parts';

export interface ManageTableProps {
  dataSourceName: string;
  table: Table;
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

const STARTING_TAB = 'Modify';

const availableTabs = (props: ManageTableProps) => [
  {
    value: 'Browse',
    label: 'Browse',
    content: <BrowseRowsContainer {...props} />,
  },
  {
    value: 'Modify',
    label: 'Modify',
    content: <ModifyTable {...props} />,
  },
  {
    value: 'Relationships',
    label: 'Relationships',
    content: <FeatureNotImplemented />,
  },
  {
    value: 'Permissions',
    label: 'Permissions',
    content: <FeatureNotImplemented />,
  },
];

export const ManageTable: React.VFC<ManageTableProps> = props => {
  const { table, dataSourceName } = props;

  const { data: databaseHierarchy, isLoading } =
    useDatabaseHierarchy(dataSourceName);

  const [tab, setTab] = useState(STARTING_TAB);

  const tableName = databaseHierarchy
    ? getTableName(table, databaseHierarchy)
    : '';

  if (!databaseHierarchy)
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
        <Tabs value={tab} onValueChange={setTab} items={availableTabs(props)} />
      </div>
    </div>
  );
};
