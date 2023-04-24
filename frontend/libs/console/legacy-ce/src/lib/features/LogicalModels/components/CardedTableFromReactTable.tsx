import { Table, flexRender } from '@tanstack/react-table';
import React from 'react';
import { CardedTable } from '../../../new-components/CardedTable';

export function CardedTableFromReactTable<T>({ table }: { table: Table<T> }) {
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
      </CardedTable.TableBody>
    </CardedTable.Table>
  );
}
