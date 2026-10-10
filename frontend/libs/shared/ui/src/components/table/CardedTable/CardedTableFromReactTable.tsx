import { RowData, Table, flexRender } from '@tanstack/react-table';
import React from 'react';
import { CoreTableFeatures } from '../DataTable/tableFeatures';
import { Table as ThemeTable } from '../Table';

export function CardedTableFromReactTable<T extends RowData>({
  table,
  noRowsMessage,
  dataTestId,
}: {
  table: Table<CoreTableFeatures, T>;
  noRowsMessage?: string;
  dataTestId?: string;
}) {
  return (
    <ThemeTable.Root data-testid={`${dataTestId}`}>
      <ThemeTable.Header>
        {table.getHeaderGroups().map((headerGroup, index) => (
          <ThemeTable.Row
            key={headerGroup.id}
            data-testid={`${dataTestId}-header-${index}`}
          >
            {headerGroup.headers.map((header) => (
              <ThemeTable.RowHeaderCell key={header.id}>
                {header.isPlaceholder
                  ? null
                  : flexRender(
                      header.column.columnDef.header,
                      header.getContext(),
                    )}
              </ThemeTable.RowHeaderCell>
            ))}
          </ThemeTable.Row>
        ))}
      </ThemeTable.Header>
      <ThemeTable.Body>
        {table.getRowModel().rows.map((row, index) => (
          <ThemeTable.Row
            key={row.id}
            data-testid={`${dataTestId}-row-${index}`}
          >
            {row.getVisibleCells().map((cell, subIndex) => (
              <ThemeTable.Cell
                key={cell.id}
                data-testid={`${dataTestId}-cell-${index}-${subIndex}`}
              >
                {flexRender(cell.column.columnDef.cell, cell.getContext())}
              </ThemeTable.Cell>
            ))}
          </ThemeTable.Row>
        ))}
        {table.getRowModel().rows.length === 0 && (
          <ThemeTable.Row>
            <ThemeTable.Cell colSpan={table.getAllColumns().length}>
              {noRowsMessage || 'No data to display'}
            </ThemeTable.Cell>
          </ThemeTable.Row>
        )}
      </ThemeTable.Body>
    </ThemeTable.Root>
  );
}

/**
 * When using React.forwardRef it's not possible to pass a Type argument to your props, but our Table component requires one
 * This function is a HOC that accepts a type argument and returns a forwardRef component that passes the type argument to the props
 * An example of where this is needed is a case when a ref is necessary to be able to correctly configure a portal for a dropdown that lives within the table
 *
 * Call this once at module scope per concrete row type (the type argument is always known at compile
 * time), not inside a component body — components must not be created during render.
 */
export function createCardedTableFromReactTableWithRef<T extends RowData>() {
  return React.forwardRef<
    HTMLDivElement,
    {
      table: Table<CoreTableFeatures, T>;
      noRowsMessage?: string;
      dataTestId?: string;
    }
  >(({ table, noRowsMessage, dataTestId }, ref) => {
    return (
      <div ref={ref}>
        <CardedTableFromReactTable
          table={table}
          noRowsMessage={noRowsMessage}
          dataTestId={dataTestId}
        />
      </div>
    );
  });
}
