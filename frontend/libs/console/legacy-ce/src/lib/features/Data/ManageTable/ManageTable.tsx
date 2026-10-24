import React, { type JSX } from 'react';
import { useLocation, useNavigate, useParams } from 'react-router';
import {
  TypedObjectValidator,
  isObject,
  isTypedObject,
  getTableDisplayName,
  dataRoutes,
} from '@hasura/shared/utils';
import { IndicatorCard, Tabs } from '@hasura/shared/ui';
import {
  useDriverCapabilities,
  useIsTableView,
} from '@hasura/metadata/data-source';
import { DatabaseRelationships } from '../../DatabaseRelationships';
import { InsertRowFormContainer } from '../InsertRow/InsertRowFormContainer';
import { getCloneRow } from '../InsertRow/cloneRow';
import { PermissionsTab } from '../../Permissions';
import { MetadataTable, Source, Table } from '@hasura/shared/types';
import { ModifyTable } from '../ModifyTable/ModifyTable';
import { useTableDefinition } from '../hooks';
import { EnabledTabs, useEnabledTabs } from '../hooks/useEnabledTabs';
import { TableBreadcrumbs, TableName } from './parts';
import { BrowseRowsContainer } from '../BrowseRows';
import { useMetadata } from '@hasura/metadata/api';
import { areTablesEqual, MetadataSelectors } from '@hasura/metadata/helpers';
import { Skeleton } from '@radix-ui/themes';

export type ManageTableTabs =
  'modify' | 'browse' | 'relationships' | 'permissions';

type Tab = {
  value: string;
  label: string;
  content: JSX.Element;
};

const isTabValidator: TypedObjectValidator = (_item) => {
  return 'value' in _item && 'label' in _item && 'content' in _item;
};

type InsertRowOptions = {
  /** the row to prefill the form with, when cloning one */
  cloneRow?: Record<string, unknown>;
  /** remounts the form when another row is cloned */
  key: string;
  onCloned: () => void;
};

const availableTabs = (
  source: Source,
  table: MetadataTable,
  areMutationsSupported: boolean,
  enabledTabs: EnabledTabs,
  isView: boolean,
  insertRow: InsertRowOptions,
): Tab[] => {
  return [
    {
      value: 'browse',
      label: 'Browse',
      content: <BrowseRowsContainer source={source} table={table.table} />,
    },
    areMutationsSupported && enabledTabs.insert && !isView
      ? {
          value: 'insert',
          label: 'Insert Row',
          content: (
            <InsertRowFormContainer
              source={source}
              key={`${JSON.stringify(table)}-${insertRow.key}`}
              table={table.table}
              {...(insertRow.cloneRow && {
                initialValues: insertRow.cloneRow,
                submitLabel: 'Clone row',
                onSuccess: insertRow.onCloned,
              })}
            />
          ),
        }
      : null,
    {
      value: 'modify',
      label: 'Modify',
      content: <ModifyTable source={source} table={table} isView={isView} />,
    },
    {
      value: 'relationships',
      label: 'Relationships',
      content: <DatabaseRelationships table={table.table} source={source} />,
    },
    {
      value: 'permissions',
      label: 'Permissions',
      content: <PermissionsTab source={source} table={table} />,
    },
  ].filter(
    (item): item is Tab =>
      isTypedObject<Tab>(item, isTabValidator) &&
      enabledTabs[item.value as keyof EnabledTabs],
  );
};

export const ManageTable: React.FC = () => {
  const urlData = useTableDefinition();

  if (urlData.querystringParseResult === 'error' || !urlData.data.table) {
    return <NotFoundComponent />;
  }

  const { database: dataSourceName, table } = urlData.data;

  return <ManageTableUI dataSourceName={dataSourceName} table={table} />;
};

const NotFoundComponent = () => (
  <IndicatorCard status="negative" showIcon>
    Could not fetch the database hierarchy for the table.
  </IndicatorCard>
);

const ManageTableUI = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const navigate = useNavigate();
  const location = useLocation();
  const { operation } = useParams<{ operation: ManageTableTabs }>();
  const {
    data: source,
    isLoading: isLoadingSource,
    isError: isSourceError,
  } = useMetadata(MetadataSelectors.findSource(dataSourceName));
  const { data: isTableView, isLoading: isLoadingTableView } = useIsTableView({
    source,
    table,
  });

  const { data: capabilities, isLoading: isLoadingCapabilities } =
    useDriverCapabilities({
      source,
    });

  const areInsertMutationsSupported =
    isObject(capabilities) && !!capabilities?.mutations?.insert;

  const enabledTabs = useEnabledTabs(dataSourceName);

  // Only block on the first load. Background refetches (e.g. after a column
  // change invalidates the source) must not unmount the tabs, or any open
  // dialog is torn down mid-submit and every section remounts and refetches.
  const isLoading =
    isLoadingSource || isLoadingCapabilities || isLoadingTableView;

  const metadataTable = table
    ? source?.tables.find((t) => areTablesEqual(t.table, table))
    : undefined;

  if (isLoading) {
    return <Skeleton height="100px" />;
  }

  if (isSourceError || !metadataTable || !source) {
    return <NotFoundComponent />;
  }

  const tabItems = availableTabs(
    source,
    metadataTable,
    areInsertMutationsSupported,
    enabledTabs,
    isTableView ?? false,
    {
      // the row menu's "Clone row" opens the Insert Row tab with the row
      cloneRow: getCloneRow(location.state),
      key: location.key,
      onCloned: () =>
        navigate(
          dataRoutes.manageTable(dataSourceName, metadataTable.table, 'browse'),
        ),
    },
  );

  const tableName = getTableDisplayName(table);

  return (
    <div className="w-full">
      <div className="p-6">
        <TableBreadcrumbs dataSourceName={dataSourceName} table={table} />
        <TableName
          source={source}
          table={metadataTable.table}
          tableName={tableName}
        />
        <Tabs
          value={operation ?? 'tables'}
          onValueChange={(_operation) => {
            navigate(
              dataRoutes.manageTable(
                dataSourceName,
                metadataTable.table,
                _operation,
              ),
            );
          }}
          items={tabItems}
        />
      </div>
    </div>
  );
};
