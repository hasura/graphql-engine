import { TableRow } from '@/features/DataSource';
import { CardedTable } from '@/new-components/CardedTable';
import { FaCaretDown, FaCaretUp } from 'react-icons/fa';
import {
  ColumnSort,
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import clsx from 'clsx';

interface ReactTableWrapperProps {
  rows: TableRow[];

  sort?: {
    sorting: ColumnSort[];
    setSorting: React.Dispatch<React.SetStateAction<ColumnSort[]>>;
  };
}

export const ReactTableWrapper = (props: ReactTableWrapperProps) => {
  const { rows, sort } = props;

  const columns = Object.keys(rows[0]);

  const columnHelper = createColumnHelper<TableRow>();

  const tableColumns = columns.map(column =>
    columnHelper.accessor(row => row[column], {
      id: column,
      cell: (info: any) => info.getValue() ?? '',
      header: () => <span key={column}>{column}</span>,
    })
  );

  const ReactTable = useReactTable<TableRow>({
    data: rows,
    columns: tableColumns,
    getCoreRowModel: getCoreRowModel(),
    state: {
      sorting: sort?.sorting,
    },
    onSortingChange: sort?.setSorting,
    manualPagination: true,
  });
  return (
    <CardedTable.Table
      className="overflow-y-auto"
      style={{ maxHeight: '650px' }}
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
            {row.getVisibleCells().map((cell, i) => (
              <CardedTable.TableBodyCell key={i}>
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </CardedTable.TableBodyCell>
            ))}
          </CardedTable.TableBodyRow>
        ))}
      </CardedTable.TableBody>
    </CardedTable.Table>
  );
};

ReactTableWrapper.defaultProps = {
  sort: undefined,
};
