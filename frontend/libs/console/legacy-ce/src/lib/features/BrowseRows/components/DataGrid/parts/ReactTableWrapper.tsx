import { TableColumn, TableRow } from '../../../../DataSource';
import { CardedTable } from '../../../../../new-components/CardedTable';
import {
  FaCaretDown,
  FaCaretUp,
  FaExternalLinkAlt,
  FaLink,
} from 'react-icons/fa';
import {
  ColumnDef,
  ColumnSort,
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React, { useEffect, useMemo, useState } from 'react';
import clsx from 'clsx';
import { Relationship } from '../../../../DatabaseRelationships';
import { CheckboxItem } from './CheckboxItem';
import { RowOptionsButton } from './RowOptionsButton';
import { RowDialog } from './RowDialog';

interface ReactTableWrapperProps {
  isRowsSelectionEnabled: boolean;
  onRowsSelect: (rows: Record<number, boolean>) => void;
  onRowDelete: (row: Record<string, any>) => void;
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
}

const renderColumnData = (data: any) => {
  if (['bigint', 'string', 'number'].includes(typeof data)) return data;

  return JSON.stringify(data);
};

export const ReactTableWrapper: React.VFC<ReactTableWrapperProps> = ({
  isRowsSelectionEnabled,
  onRowsSelect,
  onRowDelete,
  relationships,
  rows,
  sort,
  tableColumns: _tableColumns,
}) => {
  const [currentActiveRow, setCurrentActiveRow] = useState<Record<
    string,
    any
  > | null>(null);

  const columns = Object.keys(rows?.[0] ?? []);

  const columnHelper = createColumnHelper<TableRow>();

  const tableColumns = columns.map(column =>
    columnHelper.accessor(row => row[column], {
      id: column,
      header: () => <span key={column}>{column}</span>,
      enableSorting: true,
      enableMultiSort: true,
      cell: (info: any) => <>{renderColumnData(info.getValue())}</> ?? '',
    })
  );

  const [rowSelection, setRowSelection] = useState<Record<number, boolean>>({});
  useEffect(() => {
    onRowsSelect(rowSelection);
  }, [rowSelection]);

  const relationshipColumns = useMemo(
    () =>
      (relationships?.allRelationships ?? []).reduce<
        ColumnDef<TableRow, TableRow>[]
      >((cols, relationship) => {
        if (relationship.type !== 'localRelationship') return cols;

        return [
          ...cols,
          columnHelper.accessor(row => row, {
            id: relationship.name,
            header: () => (
              <span
                className="flex items-center gap-0.5"
                key={relationship.name}
              >
                <FaLink />
                {relationship.name}
              </span>
            ),
            cell: (info: any) =>
              (relationships?.activeRelationships ?? []).includes(
                relationship.name
              ) ? (
                <span
                  className="text-red-600 cursor-pointer"
                  onClick={() => relationships?.onClose(relationship.name)}
                  data-testid={`@view-relationship-${relationship.name}-goto-link`}
                >
                  <FaExternalLinkAlt />
                </span>
              ) : (
                <a
                  onClick={() => {
                    relationships?.onClick({
                      relationship,
                      rowData: info.row.original,
                    });
                  }}
                  className="cursor-pointer"
                  data-testid={`@view-relationship-${relationship.name}`}
                >
                  View
                </a>
              ),
          }),
        ];
      }, []),
    [columnHelper, relationships]
  );

  const relationshipNames =
    relationships?.allRelationships.map(rel => rel.name) ?? [];

  const ReactTable = useReactTable<TableRow>({
    data: rows,
    columns: [
      {
        id: 'selected',
        enableSorting: false,
        enableMultiSort: false,
        header: ({ table }) => (
          <div
            className="px-1 flex justify-end"
            style={{ paddingLeft: '15px' }}
          >
            <CheckboxItem
              {...{
                isChecked: table.getIsAllRowsSelected(),
                isIndeterminate: table.getIsSomeRowsSelected(),
                onChange: table.getToggleAllRowsSelectedHandler(),
              }}
              disabled={!isRowsSelectionEnabled}
            />
          </div>
        ),
        cell: ({ row }) => (
          <div className="px-1 flex items-center justify-between group-hover:opacity-100">
            <RowOptionsButton
              row={row.original}
              onOpen={(r: any) => setCurrentActiveRow(r)}
              onDelete={onRowDelete}
            />
            <CheckboxItem
              {...{
                isChecked: row.getIsSelected(),
                isIndeterminate: row.getIsSomeSelected(),
                onChange: row.getToggleSelectedHandler(),
              }}
              disabled={!isRowsSelectionEnabled}
            />
          </div>
        ),
      },
      ...tableColumns,
      ...relationshipColumns,
    ],
    onRowSelectionChange: setRowSelection,
    getCoreRowModel: getCoreRowModel(),
    state: {
      sorting: sort?.sorting,
      rowSelection,
    },
    onSortingChange: sort?.setSorting,
    manualPagination: true,
  });

  if (!rows.length)
    return (
      <div className="w-full p-3 text-center font-lg text-muted bg-white border border-gray-300">
        No rows Available
      </div>
    );

  return (
    <>
      <CardedTable.Table
        className="overflow-y-auto !rounded-t-none border-t-0"
        style={{
          maxHeight: '65vh',
          borderTopLeftRadius: '0',
          borderTopRightRadius: '0',
        }}
      >
        <CardedTable.TableHead>
          {ReactTable.getHeaderGroups().map((headerGroup, id) => (
            <CardedTable.TableHeadRow key={`${headerGroup.id}-${id}`}>
              {headerGroup.headers.map((header, i) => (
                <CardedTable.TableHeadCell key={`cell-${i}`}>
                  {header.isPlaceholder ? null : (
                    <div
                      onClick={header.column.getToggleSortingHandler()}
                      className={clsx(
                        relationshipNames.includes(header.id)
                          ? 'pointer-events-none'
                          : 'pointer-events-auto',
                        header.column.getCanSort()
                          ? 'cursor-pointer select-none flex gap-4 items-center'
                          : ''
                      )}
                      key={`header-item-${header.id}`}
                    >
                      {flexRender(
                        header.column.columnDef.header,
                        header.getContext()
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
                </CardedTable.TableHeadCell>
              ))}
            </CardedTable.TableHeadRow>
          ))}
        </CardedTable.TableHead>

        <CardedTable.TableBody>
          {ReactTable.getRowModel().rows.map(row => (
            <CardedTable.TableBodyRow
              key={row.id}
              data-testid={`@table-row-${row.id}`}
            >
              {row.getVisibleCells().map((cell, i) => {
                if (i === 0)
                  return (
                    <td
                      style={{ width: '75px', paddingLeft: '0px' }}
                      className="px-sm py-xs whitespace-nowrap text-muted overflow-hidden text-ellipsis"
                    >
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext()
                      )}
                    </td>
                  );

                return (
                  <CardedTable.TableBodyCell
                    key={`${row.id}-${i}`}
                    data-testid={`@table-cell-${row.id}-${i}`}
                    style={{ maxWidth: '20ch' }}
                  >
                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                  </CardedTable.TableBodyCell>
                );
              })}
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
      {currentActiveRow && (
        <RowDialog
          row={currentActiveRow}
          onClose={() => setCurrentActiveRow(null)}
          columns={_tableColumns}
        />
      )}
    </>
  );
};

ReactTableWrapper.defaultProps = {
  sort: undefined,
};
