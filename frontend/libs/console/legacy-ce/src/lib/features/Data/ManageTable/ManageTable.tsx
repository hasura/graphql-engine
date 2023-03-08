import { BrowseRowsContainer } from '../../BrowseRows';
import { DatabaseRelationships } from '../../DatabaseRelationships';
import { getTableName } from '../../DataSource';
import { PermissionsTab } from '../../Permissions';
import { Table } from '../../hasura-metadata-types';
import { IndicatorCard } from '../../../new-components/IndicatorCard';
import { Tabs } from '../../../new-components/Tabs';
import { getRoute } from '..';
import React from 'react';
import { useDispatch } from 'react-redux';
import { useDatabaseHierarchy, useTableDefinition } from '../hooks';
import { ModifyTable } from '../ModifyTable/ModifyTable';
import { Breadcrumbs, TableName } from './parts';
import _push from '../../../components/Services/Data/push';
import { InsertRowFormContainer } from '../../InsertRow/InsertRowFormContainer';

type AllowedTabs = 'modify' | 'browse' | 'relationship' | 'permissions';
export interface ManageTableProps {
  params: {
    operation: AllowedTabs;
  };
}

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
  // NOTE: uncomment this part to enable the new Insert Row tab
  /* {
    value: 'insert-row',
    label: 'Insert Row',
    content: (
      <InsertRowFormContainer dataSourceName={dataSourceName} table={table} />
    ),
  }, */
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
      <DatabaseRelationships dataSourceName={dataSourceName} table={table} />
    ),
  },
  {
    value: 'permissions',
    label: 'Permissions',
    content: <PermissionsTab dataSourceName={dataSourceName} table={table} />,
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
