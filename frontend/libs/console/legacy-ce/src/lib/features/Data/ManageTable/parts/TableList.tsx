import React, { useState } from 'react';
import { FaFilter } from 'react-icons/fa';
import {
  Button,
  IndicatorCard,
  hasuraToast,
  DropdownMenu,
  Table,
  showErrorNotification,
} from '@hasura/shared/ui';
import { TrackableListMenu } from '../../TrackResources/components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../TrackResources/hooks';
import { filterByTableType, filterByText } from '../../TrackResources/utils';
import {
  TrackableTable,
  useTrackTables,
  useUntrackTables,
} from '@hasura/metadata/api';
import { TableRow } from './TableRow';
import { QualifiedDataSource } from '@hasura/shared/types';

interface TableListProps {
  source: QualifiedDataSource;
  tables: TrackableTable[];
  viewingTablesThatAre: 'tracked' | 'untracked';
  onChange?: () => void;
  onMultipleTablesTrack?: () => void;
  defaultFilter?: string;
  onSingleTableTrack?: (table: TrackableTable) => void;
  trackMultipleEnabled: boolean;
}

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
  source,
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
    [typeCounts],
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
    [selectedTableTypes],
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

  const { trackTables, isPending: trackLoading } = useTrackTables();
  const { untrackTables, isPending: untrackLoading } = useUntrackTables();

  const verb = viewingTablesThatAre === 'untracked' ? 'tracked' : 'untracked';
  const action =
    viewingTablesThatAre === 'untracked' ? trackTables : untrackTables;

  const handleCheckAction = async () => {
    //make a copy of the current counts to have an accurate copy of what it was prior to the track/untrack
    const currentCounts = { ...typeCounts };
    // count the items by type in the payload
    const actionCounts = countByType(getCheckedTables());

    action(
      {
        tables: getCheckedTables(),
        source: source.name,
      },
      {
        onSuccess: () => {
          // create an array of item types where the number tracked/untracked is the same as the total (user tracked/untracked ALL of that type)
          const toRemove = Object.entries(actionCounts).reduce<string[]>(
            (prev, [key, value]) => {
              if (value === currentCounts[key]) {
                prev = [...prev, key];
              }
              return prev;
            },
            [],
          );

          // if we found any, filter them out of the selectedTableTypes
          if (toRemove.length > 0) {
            setSelectedTableTypes((prev) =>
              prev.filter((t) => !toRemove.includes(t)),
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
        onError: (err) => {
          showErrorNotification({
            title: `Failed to ${verb} tables`,
            error: err,
          });
        },
      },
    );
  };

  const onTableRowTableTrack = (table: TrackableTable) => {
    onSingleTableTrack?.(table);
    onChange?.();
  };

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
        isLoading={trackLoading || untrackLoading}
        searchChildren={
          <DropdownMenu.Root
            items={[
              <DropdownMenu.Label key="table-types-label">
                Table Types:
              </DropdownMenu.Label>,
              ...availableTableTypes.map((tableType) => (
                <DropdownMenu.CheckboxItem
                  key={tableType}
                  onCheckedChange={() => {
                    if (selectedTableTypes.includes(tableType))
                      setSelectedTableTypes((t) =>
                        t.filter((x) => x !== tableType),
                      );
                    else setSelectedTableTypes((t) => [...t, tableType]);
                  }}
                  checked={selectedTableTypes.includes(tableType)}
                >
                  {tableType} ({typeCounts[tableType]})
                </DropdownMenu.CheckboxItem>
              )),
            ]}
          >
            <Button leftIcon={FaFilter} mode="default">
              {selectedTableTypes.length ? (
                <>Type ({selectedTableTypes.length} selected)</>
              ) : (
                <>No Filters applied</>
              )}
            </Button>
          </DropdownMenu.Root>
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
        <Table.Root variant="surface">
          <Table.Header>
            <Table.Row>
              {trackMultipleEnabled && (
                <Table.RowHeaderCell>{checkAllElement()}</Table.RowHeaderCell>
              )}
              <Table.RowHeaderCell>Table</Table.RowHeaderCell>
              <Table.RowHeaderCell>Type</Table.RowHeaderCell>
              <Table.RowHeaderCell>Actions</Table.RowHeaderCell>
            </Table.Row>
          </Table.Header>

          <Table.Body>
            {paginatedTables.map((table) => (
              <TableRow
                key={table.id}
                table={table}
                source={source}
                checked={checkedIds.includes(table.id)}
                reset={resetCheckboxes}
                onChange={() => onCheck(table.id)}
                onTableTrack={onTableRowTableTrack}
                isRowSelectionEnabled={trackMultipleEnabled}
              />
            ))}
          </Table.Body>
        </Table.Root>
      )}
    </div>
  );
};
