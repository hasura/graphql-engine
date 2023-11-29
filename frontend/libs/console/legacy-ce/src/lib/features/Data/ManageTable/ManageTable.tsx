import React from 'react';
import { useDispatch } from 'react-redux';
import { getRoute } from '..';
import {
  TypedObjectValidator,
  isObject,
  isTypedObject,
} from '../../../components/Common/utils/jsUtils';
import _push from '../../../components/Services/Data/push';
import { IndicatorCard } from '../../../new-components/IndicatorCard';
import { Tabs } from '../../../new-components/Tabs';
import { sessionStore } from '../../../utils/sessionStorage';
import { BrowseRowsContainer } from '../../BrowseRows';
import { getTableName } from '../../DataSource';
import { DatabaseRelationships } from '../../DatabaseRelationships';
import { InsertRowFormContainer } from '../../InsertRow/InsertRowFormContainer';
import { PermissionsTab } from '../../Permissions';
import { Table } from '../../hasura-metadata-types';
import { ModifyTable } from '../ModifyTable/ModifyTable';
import { useDatabaseHierarchy, useTableDefinition } from '../hooks';
import { useDriverCapabilities } from '../hooks/useDriverCapabilities';
import { EnabledTabs, useEnabledTabs } from '../hooks/useEnabledTabs';
import { Breadcrumbs, TableName } from './parts';

export type ManageTableTabs =
  | 'modify'
  | 'browse'
  | 'relationships'
  | 'permissions';
export interface ManageTableProps {
  params: {
    operation: ManageTableTabs;
  };
}

type Tab = {
  value: string;
  label: string;
  content: JSX.Element;
};

const isTabValidator: TypedObjectValidator = _item => {
  return 'value' in _item && 'label' in _item && 'content' in _item;
};

const availableTabs = (
  dataSourceName: string,
  table: Table,
  tableName: string,
  areMutationsSupported: boolean,
  enabledTabs: EnabledTabs
): Tab[] =>
  [
    {
      value: 'browse',
      label: 'Browse',
      content: (
        <BrowseRowsContainer
          // key is used to force remounting of the component when users switch between tables
          key={'browse-' + JSON.stringify(table)}
          dataSourceName={dataSourceName}
          table={table}
        />
      ),
    },
    areMutationsSupported && enabledTabs.insert
      ? {
          value: 'insert',
          label: 'Insert Row',
          content: (
            <InsertRowFormContainer
              dataSourceName={dataSourceName}
              key={JSON.stringify(table)}
              table={table}
            />
          ),
        }
      : null,
    {
      value: 'modify',
      label: 'Modify',
      content: (
        <ModifyTable
          key={JSON.stringify(table)}
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
        <DatabaseRelationships
          key={JSON.stringify(table)}
          dataSourceName={dataSourceName}
          table={table}
        />
      ),
    },
    {
      value: 'permissions',
      label: 'Permissions',
      content: (
        <PermissionsTab
          key={JSON.stringify(table)}
          dataSourceName={dataSourceName}
          table={table}
        />
      ),
    },
  ].filter(
    (item): item is Tab =>
      isTypedObject<Tab>(item, isTabValidator) &&
      enabledTabs[item.value as keyof EnabledTabs]
  );

export const ManageTable: React.VFC<ManageTableProps> = (
  props: ManageTableProps
) => {
  const {
    params: { operation },
  } = props;

  const urlData = useTableDefinition(window.location);
  const dispatch = useDispatch();

  console.log('>>> urlData', urlData);

  if (urlData.querystringParseResult === 'error')
    throw Error('Unable to render');

  const { database: dataSourceName, table } = urlData.data;

  const {
    data: databaseHierarchy,
    isLoading: isLoadingHierarchy,
    isError: isErrorHierarchy,
  } = useDatabaseHierarchy(dataSourceName);

  const tableName = databaseHierarchy
    ? getTableName(table, databaseHierarchy)
    : '';

  const { data: capabilities, isLoading: isLoadingCapabilities } =
    useDriverCapabilities({
      dataSourceName,
    });

  const areInsertMutationsSupported =
    isObject(capabilities) && !!capabilities?.mutations?.insert;

  const enabledTabs = useEnabledTabs(dataSourceName);

  const isLoading = isLoadingHierarchy || isLoadingCapabilities;

  if (isErrorHierarchy)
    return (
      <IndicatorCard status="negative">
        Could not fetch the database hierarchy for the table.
      </IndicatorCard>
    );

  if (isLoading) return <IndicatorCard status="info">Loading...</IndicatorCard>;

  const tabItems = availableTabs(
    dataSourceName,
    table,
    tableName,
    areInsertMutationsSupported,
    enabledTabs
  );

  return (
    <div className="w-full bg-gray-50">
      <div className="px-md pt-md mb-xs">
        <Breadcrumbs dataSourceName={dataSourceName} tableName={tableName} />
        <TableName
          dataSourceName={dataSourceName}
          tableName={tableName}
          table={table}
        />
        <Tabs
          value={operation}
          onValueChange={_operation => {
            dispatch(
              _push(getRoute().table(dataSourceName, table, _operation))
            );

            // save last tab to session storage:
            sessionStore.setItem(
              'manageTable.lastTab',
              _operation as ManageTableTabs
            );
          }}
          items={tabItems}
        />
      </div>
    </div>
  );
};
