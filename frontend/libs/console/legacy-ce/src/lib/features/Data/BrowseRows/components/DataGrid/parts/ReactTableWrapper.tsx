import { TableColumn, TableRow } from '@hasura/metadata/data-source';
import { Source, Table as HasuraTable } from '@hasura/shared/types';
import {
  IndicatorCard,
  sortableSelectableTableFeatures,
  SortableSelectableTableFeatures,
  Table,
  Text,
} from '@hasura/shared/ui';
import { Checkbox, Flex, Link } from '@radix-ui/themes';
import {
  FaCaretDown,
  FaCaretUp,
  FaExternalLinkAlt,
  FaLink,
} from 'react-icons/fa';
import {
  ColumnDef,
  ColumnSort,
  OnChangeFn,
  RowSelectionState,
  createColumnHelper,
  flexRender,
  useTable,
} from '@tanstack/react-table';
import React, { useMemo, useState } from 'react';
import clsx from 'clsx';
import { Relationship } from '../../../../../DatabaseRelationships';
import { RowOptionsButton } from './RowOptionsButton';
import { useNavigate } from 'react-router';
import { dataRoutes } from '@hasura/shared/utils';
import { RowDialog } from './RowDialog';
import { EditRowDialog } from './EditRowDialog';
import { toCloneRowState } from '../../../../InsertRow/cloneRow';

interface ReactTableWrapperProps {
  disabled: boolean;
  isRowsSelectionEnabled: boolean;
  selectedRows: RowSelectionState;
  onRowsSelect: OnChangeFn<RowSelectionState>;
  onRowDelete: (row: Record<string, any>) => void;
  /** called once a row is edited, e.g. to reload the rows */
  onRowEdited?: () => void;
  relationships?: {
    allRelationships: Relationship[];
    onClick: (props: { relationship: Relationship; rowData: TableRow }) => void;
    onClose: (relationshipName: string) => void;
    activeRelationships?: string[];
  };
  rows: TableRow[];
  sort?: {
    sorting: ColumnSort[];
    setSorting: React.Dispatch<React.SetStateAction<ColumnSort[]>>;
  };
  tableColumns: TableColumn[];
  source: Source;
  table: HasuraTable;
}

const renderColumnData = (data: any) => {
  if (['bigint', 'string', 'number'].includes(typeof data)) return data;

  return JSON.stringify(data);
};

const columnHelper = createColumnHelper<
  SortableSelectableTableFeatures,
  TableRow
>();

export const ReactTableWrapper: React.FC<ReactTableWrapperProps> = ({
  isRowsSelectionEnabled,
  onRowsSelect,
  onRowDelete,
  onRowEdited,
  relationships,
  rows,
  sort,
  tableColumns: _tableColumns,
  disabled,
  selectedRows,
  source,
  table,
}) => {
  const [currentActiveRow, setCurrentActiveRow] = useState<Record<
    string,
    any
  > | null>(null);
  const navigate = useNavigate();
  // a row is cloned in the Insert Row tab, prefilled through the router state
  const cloneRow = (row: Record<string, unknown>) =>
    navigate(dataRoutes.manageTable(source.name, table, 'insert'), {
      state: toCloneRowState(row),
    });
  const [rowToEdit, setRowToEdit] = useState<Record<string, any> | null>(null);

  // keep primary keys first (in table definition order), then sort the rest
  // by name so the column order is stable across queries
  const primaryKeyNames = _tableColumns
    .filter((column) => column.isPrimaryKey)
    .flatMap((column) => [column.name, column.graphQLProperties?.name]);
  const rowKeys = Object.keys(rows?.[0] ?? []);
  // a key column missing from the rows (e.g. CockroachDB's hidden `rowid` of
  // a table without a primary key) can't be used to target a row
  const primaryKeyColumns = _tableColumns
    .filter((column) => column.isPrimaryKey)
    .map((column) => column.name);
  const hasPrimaryKey =
    primaryKeyColumns.length > 0 &&
    primaryKeyColumns.every((name) => rowKeys.includes(name));
  const columns = [
    ...primaryKeyNames.filter(
      (name, index): name is string =>
        !!name &&
        rowKeys.includes(name) &&
        primaryKeyNames.indexOf(name) === index,
    ),
    ...rowKeys
      .filter((key) => !primaryKeyNames.includes(key))
      .sort((a, b) => a.localeCompare(b)),
  ];

  const tableColumns = columns.map((column) =>
    columnHelper.accessor((row) => row[column], {
      id: column,
      header: () => <span key={column}>{column}</span>,
      enableSorting: true,
      enableMultiSort: true,
      cell: (info: any) => {
        const value = renderColumnData(info.getValue()) ?? '';
        // primary key values open the row dialog, same as the "Open" row option
        return primaryKeyNames.includes(column) ? (
          <Link
            onClick={() => setCurrentActiveRow(info.row.original)}
            className="cursor-pointer!"
            data-testid={`@open-row-${column}`}
          >
            {value}
          </Link>
        ) : (
          <>{value}</>
        );
      },
    }),
  );

  const relationshipColumns = useMemo(
    () =>
      (relationships?.allRelationships ?? []).reduce<
        ColumnDef<SortableSelectableTableFeatures, TableRow, TableRow>[]
      >((cols, relationship) => {
        if (relationship.type !== 'localRelationship') return cols;

        return [
          ...cols,
          columnHelper.accessor((row) => row, {
            id: relationship.name,
            header: () => (
              <Flex align="center" gap="1" key={relationship.name}>
                <FaLink />
                {relationship.name}
              </Flex>
            ),
            cell: (info: any) =>
              (relationships?.activeRelationships ?? []).includes(
                relationship.name,
              ) ? (
                <Text
                  color="red"
                  className="cursor-pointer"
                  onClick={() => relationships?.onClose(relationship.name)}
                  data-testid={`@view-relationship-${relationship.name}-goto-link`}
                >
                  <FaExternalLinkAlt />
                </Text>
              ) : (
                <Link
                  onClick={() => {
                    relationships?.onClick({
                      relationship,
                      rowData: info.row.original,
                    });
                  }}
                  className="cursor-pointer!"
                  data-testid={`@view-relationship-${relationship.name}`}
                >
                  View
                </Link>
              ),
          }),
        ];
      }, []),
    [relationships],
  );

  const relationshipNames =
    relationships?.allRelationships.map((rel) => rel.name) ?? [];

  const ReactTable = useTable({
    features: sortableSelectableTableFeatures,
    data: rows,
    columns: columnHelper.columns([
      columnHelper.display({
        id: 'selected',
        enableSorting: false,
        enableMultiSort: false,
        header: ({ table }) => (
          <Flex justify="end">
            <Checkbox
              checked={
                table.getIsSomeRowsSelected()
                  ? 'indeterminate'
                  : table.getIsAllRowsSelected()
              }
              onClick={table.getToggleAllRowsSelectedHandler()}
              disabled={!isRowsSelectionEnabled || !hasPrimaryKey || disabled}
            />
          </Flex>
        ),
        cell: ({ row }) => (
          <Flex
            align="center"
            justify="between"
            className="pl-4 group-hover:opacity-100"
          >
            <RowOptionsButton
              row={row.original}
              onOpen={(r: any) => setCurrentActiveRow(r)}
              onClone={cloneRow}
              // without a primary key, the row can't be targeted
              onEdit={hasPrimaryKey ? (r: any) => setRowToEdit(r) : undefined}
              onDelete={hasPrimaryKey ? onRowDelete : undefined}
            />
            <Checkbox
              checked={row.getIsSelected()}
              onClick={row.getToggleSelectedHandler()}
              disabled={!isRowsSelectionEnabled || !hasPrimaryKey || disabled}
            />
          </Flex>
        ),
      }),
      ...tableColumns,
      ...relationshipColumns,
    ]),
    onRowSelectionChange: onRowsSelect,
    state: {
      sorting: sort?.sorting,
      rowSelection: selectedRows,
    },
    onSortingChange: sort?.setSorting,
  });

  if (!rows.length) return <IndicatorCard>No rows Available</IndicatorCard>;

  return (
    <>
      <Table.Root variant="surface" className="overflow-auto max-h-[65vh]">
        <Table.Header>
          {ReactTable.getHeaderGroups().map((headerGroup, id) => (
            <Table.Row key={`${headerGroup.id}-${id}`}>
              {headerGroup.headers.map((header, i) => (
                <Table.RowHeaderCell key={`cell-${i}`}>
                  {header.isPlaceholder ? null : (
                    <div
                      onClick={header.column.getToggleSortingHandler()}
                      className={clsx(
                        relationshipNames.includes(header.id)
                          ? 'pointer-events-none'
                          : 'pointer-events-auto',
                        header.column.getCanSort()
                          ? 'cursor-pointer select-none flex gap-4 items-center'
                          : '',
                      )}
                      key={`header-item-${header.id}`}
                    >
                      {flexRender(
                        header.column.columnDef.header,
                        header.getContext(),
                      )}
                      {header?.id === 'options-button' ||
                        (!relationshipNames.includes(header.id) &&
                          header.column.getCanSort() &&
                          ({
                            asc: <FaCaretUp />,
                            desc: <FaCaretDown />,
                          }[header.column.getIsSorted() as string] ?? (
                            <span className="flex flex-col">
                              <FaCaretUp />
                              <FaCaretDown style={{ marginTop: '-5px' }} />
                            </span>
                          )))}
                    </div>
                  )}
                </Table.RowHeaderCell>
              ))}
            </Table.Row>
          ))}
        </Table.Header>

        <Table.Body>
          {ReactTable.getRowModel().rows.map((row) => (
            <Table.Row key={row.id} data-testid={`@table-row-${row.id}`}>
              {row.getVisibleCells().map((cell, i) => {
                if (i === 0)
                  return (
                    <Table.Cell
                      key={`${row.id}-${i}`}
                      style={{ width: '75px', paddingLeft: '0px' }}
                      className="whitespace-nowrap overflow-hidden text-ellipsis"
                    >
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext(),
                      )}
                    </Table.Cell>
                  );

                return (
                  <Table.Cell
                    key={`${row.id}-${i}`}
                    data-testid={`@table-cell-${row.id}-${i}`}
                    style={{ maxWidth: '20ch' }}
                    className="whitespace-nowrap overflow-hidden text-ellipsis"
                  >
                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                  </Table.Cell>
                );
              })}
            </Table.Row>
          ))}
        </Table.Body>
      </Table.Root>
      {currentActiveRow && (
        <RowDialog
          row={currentActiveRow}
          onClose={() => setCurrentActiveRow(null)}
          columns={_tableColumns}
        />
      )}
      {rowToEdit && (
        <EditRowDialog
          source={source}
          table={table}
          row={rowToEdit}
          onClose={() => setRowToEdit(null)}
          onSaved={onRowEdited}
        />
      )}
    </>
  );
};
