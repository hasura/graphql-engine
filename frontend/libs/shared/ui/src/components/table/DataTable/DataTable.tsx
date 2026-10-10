import {
  ColumnDef,
  flexRender,
  RowData,
  useTable,
} from '@tanstack/react-table';
import { dataTableFeatures, DataTableFeatures } from './tableFeatures';
import { Pagination } from '../../pagination';
import { Table } from '../Table';

export type DataTableColumnDef<T extends RowData> = ColumnDef<
  DataTableFeatures,
  T
>;

export type DataTableProps<T extends RowData> = {
  columns: DataTableColumnDef<T>[];
  data: T[];
  pageSize?: number;
  noRowsMessage?: string;
  dataTestId?: string;
  className?: string;
};

export function DataTable<T extends RowData>({
  columns,
  data,
  pageSize = 20,
  noRowsMessage,
  dataTestId,
  className,
}: DataTableProps<T>) {
  const table = useTable({
    features: dataTableFeatures,
    data,
    columns,
    initialState: {
      pagination: { pageIndex: 0, pageSize },
    },
  });

  return (
    <div data-testid={dataTestId}>
      <Table.Root className={className}>
        <Table.Header>
          {table.getHeaderGroups().map((headerGroup) => (
            <Table.Row key={headerGroup.id}>
              {headerGroup.headers.map((header) => (
                <Table.Cell key={header.id}>
                  {header.isPlaceholder
                    ? null
                    : flexRender(
                        header.column.columnDef.header,
                        header.getContext(),
                      )}
                </Table.Cell>
              ))}
            </Table.Row>
          ))}
        </Table.Header>
        <Table.Body>
          {table.getRowModel().rows.map((row) => (
            <Table.Row key={row.id}>
              {row.getVisibleCells().map((cell) => (
                <Table.Cell key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </Table.Cell>
              ))}
            </Table.Row>
          ))}
          {table.getRowModel().rows.length === 0 && (
            <Table.Row>
              <Table.Cell colSpan={table.getAllColumns().length}>
                {noRowsMessage || 'No data to display'}
              </Table.Cell>
            </Table.Row>
          )}
        </Table.Body>
      </Table.Root>
      {table.getPageCount() > 1 && (
        <Pagination
          {...table.state.pagination}
          justify="center"
          setPageIndex={table.setPageIndex}
          setPageSize={table.setPageSize}
          pageCount={table.getPageCount()}
        />
      )}
    </div>
  );
}
