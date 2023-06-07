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
import { useDriverCapabilities } from '../hooks/useDriverCapabilities';
import {
  isObject,
  isTypedObject,
  TypedObjectValidator,
} from '../../../components/Common/utils/jsUtils';
import { EnabledTabs, useEnabledTabs } from '../hooks/useEnabledTabs';

type AllowedTabs = 'modify' | 'browse' | 'relationship' | 'permissions';
export interface ManageTableProps {
  params: {
    operation: AllowedTabs;
  };
}

type Tab = {
  value: string;
  label: string;
  content: JSX.Element;
};

const isTabValidator: TypedObjectValidator = _item =>
  'value' in _item && 'label' in _item && 'content' in _item;

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
    // NOTE: uncomment this part to enable the new Insert Row tab
    areMutationsSupported
      ? {
          value: 'insert-row',
          label: 'Insert Row',
          content: (
            <InsertRowFormContainer
              dataSourceName={dataSourceName}
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
          }}
          items={availableTabs(
            dataSourceName,
            table,
            tableName,
            areInsertMutationsSupported,
            enabledTabs
          )}
        />
      </div>
    </div>
  );
};
