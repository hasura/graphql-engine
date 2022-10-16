import { TableRow } from '@/features/DataSource';
import {
  FaArrowLeft,
  FaArrowRight,
  FaCaretDown,
  FaCaretUp,
  FaSearch,
  FaUndo,
} from 'react-icons/fa';
import { Button } from '@/new-components/Button';
import { CardedTable } from '@/new-components/CardedTable';
import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  useReactTable,
  PaginationState,
  SortingState,
} from '@tanstack/react-table';
import React, { useEffect, useState } from 'react';
import clsx from 'clsx';
import Skeleton from 'react-loading-skeleton';

interface DataTableProps {
  rows: TableRow[];
  columns: string[];
  pagination: PaginationState;
  setPagination: React.Dispatch<React.SetStateAction<PaginationState>>;
  sorting: SortingState;
  setSorting: React.Dispatch<React.SetStateAction<SortingState>>;
  onQueryClick: () => void;
  resetQuery: () => void;
  isLoading: boolean;
}

export const DataTable = ({
  rows,
  columns,
  pagination,
  sorting,
  setSorting,
  setPagination,
  onQueryClick,
  resetQuery,
  isLoading,
}: DataTableProps) => {
  const [data, setData] = useState<TableRow[]>([]);
  const columnHelper = createColumnHelper<TableRow>();

  useEffect(() => {
    setData([...rows]);
  }, [rows]);

  const tableColumns = columns.map(column =>
    columnHelper.accessor(row => row[column], {
      id: column,
      cell: (info: any) => info.getValue() ?? '',
      header: () => <span>{column}</span>,
      footer: info => info.column.id,
    })
  );

  const ReactTable = useReactTable<TableRow>({
    data,
    columns: tableColumns,
    getCoreRowModel: getCoreRowModel(),
    state: {
      pagination,
      sorting,
    },
    onSortingChange: setSorting,
    onPaginationChange: setPagination,
    manualPagination: true,
  });

  return (
    <div className="p-2">
      <div
        className="flex items-center justify-between p-4 bg-white border"
        id="query-options"
      >
        <div className="flex space-x-1.5">
          <Button
            type="button"
            mode="primary"
            icon={<FaSearch />}
            onClick={onQueryClick}
          >
            Query
          </Button>
          <Button
            type="button"
            mode="default"
            onClick={resetQuery}
            icon={<FaUndo />}
          >
            Reset
          </Button>
        </div>
        <div className="flex gap-2 items-center">
          <Button
            type="button"
            icon={<FaArrowLeft />}
            onClick={() => ReactTable.previousPage()}
            disabled={pagination.pageIndex === 0}
          />

          <select
            value={ReactTable.getState().pagination.pageSize}
            onChange={e => {
              ReactTable.setPageSize(Number(e.target.value));
            }}
            className="block w-full max-w-xl h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400"
          >
            {[10, 20, 30, 40, 50, 100].map(pageSize => (
              <option key={pageSize} value={pageSize}>
                Show {pageSize} rows
              </option>
            ))}
          </select>
          <Button
            type="button"
            icon={<FaArrowRight />}
            onClick={() => ReactTable.nextPage()}
            disabled={data.length < pagination.pageSize}
          />
        </div>
      </div>

      {isLoading ? (
        <div className="my-4">
          <Skeleton height={30} count={8} className="my-2" />
        </div>
      ) : (
        <CardedTable.Table
          className="overflow-y-auto"
          style={{ maxHeight: '650px' }}
        >
          <CardedTable.TableHead>
            {ReactTable.getHeaderGroups().map(headerGroup => (
              <CardedTable.TableHeadRow key={headerGroup.id}>
                {headerGroup.headers.map(header => (
                  <CardedTable.TableHeadCell>
                    {header.isPlaceholder ? null : (
                      <div
                        onClick={header.column.getToggleSortingHandler()}
                        className={clsx(
                          header.column.getCanSort()
                            ? 'cursor-pointer select-none flex gap-4 items-center'
                            : ''
                        )}
                      >
                        {flexRender(
                          header.column.columnDef.header,
                          header.getContext()
                        )}
                        {{
                          asc: <FaCaretUp />,
                          desc: <FaCaretDown />,
                        }[header.column.getIsSorted() as string] ?? (
                          <span className="flex flex-col">
                            <FaCaretUp />
                            <FaCaretDown style={{ marginTop: '-5px' }} />
                          </span>
                        )}
                      </div>
                    )}
                  </CardedTable.TableHeadCell>
                ))}
              </CardedTable.TableHeadRow>
            ))}
          </CardedTable.TableHead>

          <CardedTable.TableBody>
            {ReactTable.getRowModel().rows.map(row => (
              <CardedTable.TableBodyRow key={row.id}>
                {row.getVisibleCells().map(cell => (
                  <CardedTable.TableBodyCell key={cell.id}>
                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                  </CardedTable.TableBodyCell>
                ))}
              </CardedTable.TableBodyRow>
            ))}
          </CardedTable.TableBody>
        </CardedTable.Table>
      )}
    </div>
  );
};
