import React, { useMemo } from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import {
  dataTableFeatures,
  DataTableFeatures,
  getReactTablePaginationProps,
  Pagination,
  Table,
} from '@hasura/shared/ui';
import { getColWidth } from '../../../components/Common/TableCommon/DragFoldTable';
import { Heading } from '@radix-ui/themes';

interface ResultTableProps {
  headers: string[];
  rows: Array<string[]>;
}
const getColCellContent = (rowColumnValue: React.ReactNode) => {
  let cellValue: React.ReactNode = '';

  if (rowColumnValue === null) {
    cellValue = <i>NULL</i>;
  } else if (rowColumnValue === undefined) {
    cellValue = 'NULL';
  } else if (typeof rowColumnValue === 'object') {
    cellValue = JSON.stringify(rowColumnValue, null, 4);
  } else {
    cellValue = rowColumnValue.toString();
  }

  return <div>{cellValue}</div>;
};

const getTableConfig = (headers: string[], rows: Array<string[]>) => {
  const dataMap: Record<string, any>[] = [];
  const data = rows.map((i) => {
    const result: Record<string, any> = {};
    const resultMap: Record<string, any> = {};
    headers.forEach((hdr, hIndex) => {
      result[hdr] = getColCellContent(i[hIndex]);
      resultMap[hdr] = i[hIndex];
    });
    dataMap.push(resultMap);
    return result;
  });
  const columns: ColumnDef<DataTableFeatures, Record<string, any>>[] =
    headers.map((header) => ({
      id: header,
      accessorKey: header,
      header,
      size: Math.ceil(getColWidth(header, dataMap)),
      cell: (info) => <div>{info.getValue() as React.ReactNode}</div>,
    }));

  return { columns, data };
};

const ResultTable: React.FC<ResultTableProps> = ({ headers, rows }) => {
  const { columns, data } = useMemo(
    () => getTableConfig(headers, rows),
    [headers, rows],
  );

  const table = useTable({
    features: dataTableFeatures,
    data,
    columns,
    initialState: {
      pagination: {
        pageIndex: 0,
        pageSize: rows.length > 10 ? 10 : rows.length || 10,
      },
    },
  });

  return (
    <div>
      <div className="my-4">
        <Heading size="3">SQL Result:</Heading>
      </div>
      <Table.Root variant="surface">
        <Table.Header>
          {table.getHeaderGroups().map((headerGroup) => (
            <Table.Row key={headerGroup.id}>
              {headerGroup.headers.map((header) => (
                <Table.RowHeaderCell
                  key={header.id}
                  style={{ width: header.getSize() }}
                >
                  {header.isPlaceholder
                    ? null
                    : flexRender(
                        header.column.columnDef.header,
                        header.getContext(),
                      )}
                </Table.RowHeaderCell>
              ))}
            </Table.Row>
          ))}
        </Table.Header>
        <Table.Body>
          {table.getRowModel().rows.map((row) => (
            <Table.Row key={row.id}>
              {row.getVisibleCells().map((cell) => (
                <Table.Cell
                  key={cell.id}
                  style={{ width: cell.column.getSize() }}
                >
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </Table.Cell>
              ))}
            </Table.Row>
          ))}
        </Table.Body>
      </Table.Root>
      {table.getPageCount() > 1 && (
        <Pagination {...getReactTablePaginationProps(table)} justify="center" />
      )}
    </div>
  );
};

export default ResultTable;
