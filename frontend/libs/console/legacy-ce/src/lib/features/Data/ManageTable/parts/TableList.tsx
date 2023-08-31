import React, { useState } from 'react';
import { FaFilter } from 'react-icons/fa';
import { DropDown } from '../../../../new-components/AdvancedDropDown';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { hasuraToast } from '../../../../new-components/Toasts';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { manageTableUrl } from '../../../DataSidebar/navigation-utils';
import { PostgresTable } from '../../../DataSource';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '../../../FeatureFlags';
import { TrackableListMenu } from '../../TrackResources/components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../TrackResources/hooks';
import { filterByTableType, filterByText } from '../../TrackResources/utils';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';
import { useTrackTables } from '../../hooks/useTrackTables';
import { TrackableTable } from '../types';
import { TableRow } from './TableRow';

interface TableListProps {
  dataSourceName: string;
  tables: TrackableTable[];
  viewingTablesThatAre: 'tracked' | 'untracked';
  onChange?: () => void;
  onMultipleTablesTrack?: () => void;
  defaultFilter?: string;
  onSingleTableTrack?: (table: TrackableTable) => void;
  trackMultipleEnabled: boolean;
}

// const getDefaultSelectedTableType = (availableTableTypes: string[]) => {
//   return availableTableTypes.includes('BASE TABLE')
//     ? ['BASE TABLE']
//     : availableTableTypes;
// };

const countByType = (tables: TrackableTable[]) =>
  tables.reduce<Record<string, number>>((prev, current) => {
    if (prev[current.type]) {
      prev[current.type]++;
    } else {
      prev[current.type] = 1;
    }
    return prev;
  }, {});

export const TableList = ({
  viewingTablesThatAre,
  dataSourceName,
  tables,
  onChange,
  defaultFilter,
  onMultipleTablesTrack,
  onSingleTableTrack,
  trackMultipleEnabled,
}: TableListProps) => {
  const typeCounts = React.useMemo(() => countByType(tables), [tables]);

  const availableTableTypes = React.useMemo(
    () => Object.keys(typeCounts),
    [typeCounts]
  );

  const [selectedTableTypes, setSelectedTableTypes] = useState<string[]>([]);

  const searchFn = React.useCallback(
    (searchText: string, table: TrackableTable) => {
      const parentText = table.name.toLowerCase().split('.').join(' / ');
      return (
        filterByText(parentText, searchText) &&
        filterByTableType(table.type, selectedTableTypes)
      );
    },
    [selectedTableTypes]
  );
  const listProps = usePaginatedSearchableList<TrackableTable>({
    data: tables,
    filterFn: searchFn,
    defaultQuery: defaultFilter,
  });

  const {
    checkData: { onCheck, checkedIds, reset: resetCheckboxes, checkAllElement },
    paginatedData: paginatedTables,
    getCheckedItems: getCheckedTables,
  } = listProps;

  const { trackTables, isLoading, untrackTables } = useTrackTables({
    dataSourceName,
  });

  const verb = viewingTablesThatAre === 'untracked' ? 'tracked' : 'untracked';
  const action =
    viewingTablesThatAre === 'untracked' ? trackTables : untrackTables;

  const handleCheckAction = async () => {
    //make a copy of the current counts to have an accurate copy of what it was prior to the track/untrack
    const currentCounts = { ...typeCounts };
    // count the items by type in the payload
    const actionCounts = countByType(getCheckedTables());

    action({
      tables: getCheckedTables(),
      onSuccess: () => {
        // create an array of item types where the number tracked/untracked is the same as the total (user tracked/untracked ALL of that type)
        const toRemove = Object.entries(actionCounts).reduce<string[]>(
          (prev, [key, value]) => {
            if (value === currentCounts[key]) {
              prev = [...prev, key];
            }
            return prev;
          },
          []
        );

        // if we found any, filter them out of the selectedTableTypes
        if (toRemove.length > 0) {
          setSelectedTableTypes(prev =>
            prev.filter(t => !toRemove.includes(t))
          );
        }

        resetCheckboxes();

        hasuraToast({
          type: 'success',
          title: `Successfully ${verb}`,
          message: `${getCheckedTables().length} ${
            getCheckedTables().length <= 1 ? 'table' : 'tables'
          } ${verb}!`,
        });
        onMultipleTablesTrack?.();
        onChange?.();
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: err.name,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
  };

  const pushRoute = usePushRoute();

  const onTableRowTableTrack = (table: TrackableTable) => {
    onSingleTableTrack?.(table);
    onChange?.();
  };

  const { enabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.performanceMode
  );

  const onTableRowTableNameClick = (table: TrackableTable) =>
    viewingTablesThatAre === 'tracked'
      ? () => {
          if (!enabled && 'schema' in (table.table as any)) {
            const { name, schema } = table.table as PostgresTable;

            pushRoute(
              `/data/${dataSourceName}/schema/${schema}/tables/${name}/modify`
            );
          } else
            pushRoute(manageTableUrl({ dataSourceName, table: table.table }));
        }
      : undefined;

  if (!tables.length) {
    return (
      <div className="space-y-4">
        <IndicatorCard>{`No ${
          viewingTablesThatAre === 'tracked' ? 'tracked' : 'untracked'
        } tables found`}</IndicatorCard>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      <TrackableListMenu
        checkActionText={`${
          viewingTablesThatAre === 'tracked' ? 'Untrack' : 'Track'
        } Selected (${getCheckedTables().length})`}
        handleTrackButton={() => {
          handleCheckAction();
        }}
        showButton={trackMultipleEnabled}
        isLoading={isLoading}
        searchChildren={
          <DropDown.Root
            trigger={
              <Button icon={<FaFilter />}>
                {selectedTableTypes.length ? (
                  <>Type ({selectedTableTypes.length} selected)</>
                ) : (
                  <>No Filters applied</>
                )}
              </Button>
            }
          >
            <DropDown.Label>Table Types:</DropDown.Label>
            <>
              {availableTableTypes.map(tableType => (
                <DropDown.CheckItem
                  key={tableType}
                  onCheckChange={() => {
                    if (selectedTableTypes.includes(tableType))
                      setSelectedTableTypes(t =>
                        t.filter(x => x !== tableType)
                      );
                    else setSelectedTableTypes(t => [...t, tableType]);
                  }}
                  checked={selectedTableTypes.includes(tableType)}
                >
                  <div>
                    {tableType} ({typeCounts[tableType]})
                  </div>
                </DropDown.CheckItem>
              ))}
            </>
          </DropDown.Root>
        }
        {...listProps}
      />

      {!paginatedTables.length ? (
        <div className="space-y-4">
          <IndicatorCard>{`No ${
            viewingTablesThatAre === 'tracked' ? 'tracked' : 'untracked'
          } tables found found for the applied filter`}</IndicatorCard>
        </div>
      ) : (
        <CardedTable.Table>
          <CardedTable.TableHead>
            <CardedTable.TableHeadRow>
              {trackMultipleEnabled && (
                <th className="w-0 bg-gray-50 px-sm text-sm font-semibold text-muted uppercase tracking-wider border-r">
                  {checkAllElement()}
                </th>
              )}
              <CardedTable.TableHeadCell>Table</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Type</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Actions</CardedTable.TableHeadCell>
            </CardedTable.TableHeadRow>
          </CardedTable.TableHead>

          <CardedTable.TableBody>
            {paginatedTables.map(table => (
              <TableRow
                key={table.id}
                table={table}
                dataSourceName={dataSourceName}
                checked={checkedIds.includes(table.id)}
                reset={resetCheckboxes}
                onChange={() => onCheck(table.id)}
                onTableTrack={onTableRowTableTrack}
                onTableNameClick={onTableRowTableNameClick(table)}
                isRowSelectionEnabled={trackMultipleEnabled}
              />
            ))}
          </CardedTable.TableBody>
        </CardedTable.Table>
      )}
      <style
        // fixes double scroll bar issue on page:
        dangerouslySetInnerHTML={{
          __html: `div[class^="RightContainer_main"] { overflow: unset; }`,
        }}
      />
    </div>
  );
};
