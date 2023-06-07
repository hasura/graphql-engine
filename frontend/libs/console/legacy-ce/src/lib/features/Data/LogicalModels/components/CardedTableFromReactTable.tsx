import { Table, flexRender } from '@tanstack/react-table';
import { CardedTable } from '../../../../new-components/CardedTable';
import React from 'react';

export function CardedTableFromReactTable<T>({
  table,
  noRowsMessage,
}: {
  table: Table<T>;
  noRowsMessage?: string;
}) {
  return (
    <CardedTable.Table>
      <CardedTable.TableHead>
        {table.getHeaderGroups().map(headerGroup => (
          <CardedTable.TableHeadRow key={headerGroup.id}>
            {headerGroup.headers.map(header => (
              <CardedTable.TableHeadCell key={header.id}>
                {header.isPlaceholder
                  ? null
                  : flexRender(
                      header.column.columnDef.header,
                      header.getContext()
                    )}
              </CardedTable.TableHeadCell>
            ))}
          </CardedTable.TableHeadRow>
        ))}
      </CardedTable.TableHead>
      <CardedTable.TableBody>
        {table.getRowModel().rows.map(row => (
          <CardedTable.TableBodyRow key={row.id}>
            {row.getVisibleCells().map(cell => (
              <CardedTable.TableBodyCell key={cell.id}>
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </CardedTable.TableBodyCell>
            ))}
          </CardedTable.TableBodyRow>
        ))}
        {table.getRowModel().rows.length === 0 && (
          <tr>
            <td
              className="p-3 text-muted"
              colSpan={table.getAllColumns().length}
            >
              {noRowsMessage || 'No data to display'}
            </td>
          </tr>
        )}
      </CardedTable.TableBody>
    </CardedTable.Table>
  );
}

/**
 * When using React.forwardRef it's not possible to pass a Type argument to your props, but our Table component requires one
 * This function is a HOC that accepts a type argument and returns a forwardRef component that passes the type argument to the props
 * An example of where this is needed is a case when a ref is necessary to be able to correctly configure a portal for a dropdown that lives within the table
 */
function forwardRefWrapper<T>() {
  return React.forwardRef<
    HTMLDivElement,
    { table: Table<T>; noRowsMessage?: string }
  >(({ table, noRowsMessage }, ref) => {
    return (
      <div ref={ref}>
        <CardedTableFromReactTable
          table={table}
          noRowsMessage={noRowsMessage}
        />
      </div>
    );
  });
}

/**
 * In order to prevent unwanted re-renders when using the forwardRefWrapper in a component, it needs to be wrapped in useMemo (especially if form elements are inside)
 * This hook takes care of this so the implementation does not need to
 */
export function useCardedTableFromReactTableWithRef<TableType>() {
  const Table = React.useMemo(() => forwardRefWrapper<TableType>(), []);
  return Table;
}
