import React, { useMemo, useState } from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import {
  dataTableFeatures,
  DataTableFeatures,
  getReactTablePaginationProps,
  IconButton,
  Pagination,
  Table,
} from '@hasura/shared/ui';
import { getColWidth } from '../../../components/Common/TableCommon/DragFoldTable';
import { Heading } from '@radix-ui/themes';
import { ResultRowDialog } from './ResultRowDialog';
import { FaExpand } from 'react-icons/fa6';

interface ResultTableProps {
  headers: string[];
  rows: Array<string[]>;
}
// long values are cut to one line at the column width; the full value is
// shown on hover
const getColCellContent = (
  rowColumnValue: React.ReactNode,
  maxWidth: number,
) => {
  let cellValue: React.ReactNode;

  if (rowColumnValue === null) {
    cellValue = <i>NULL</i>;
  } else if (rowColumnValue === undefined) {
    cellValue = 'NULL';
  } else if (typeof rowColumnValue === 'object') {
    cellValue = JSON.stringify(rowColumnValue, null, 4);
  } else {
    cellValue = rowColumnValue.toString();
  }

  return (
    <div
      className="truncate"
      style={{ maxWidth }}
      title={typeof cellValue === 'string' ? cellValue : undefined}
    >
      {cellValue}
    </div>
  );
};

const OPEN_ROW_COLUMN_SIZE = 40;

const getTableConfig = (
  headers: string[],
  rows: Array<string[]>,
  onOpenRow: (index: number) => void,
) => {
  const sizes = headers.map((header, hIndex) =>
    Math.ceil(
      getColWidth(
        header,
        rows.map((i) => i[hIndex]),
      ),
    ),
  );
  const columns: ColumnDef<DataTableFeatures, string[]>[] = [
    // opens the row in full, as cells are cut to the column width
    {
      id: 'open-row',
      header: '',
      size: OPEN_ROW_COLUMN_SIZE,
      cell: (info) => (
        <IconButton
          color="gray"
          variant="ghost"
          radius="full"
          title="Open row"
          aria-label="Open row"
          icon={FaExpand}
          onClick={() => onOpenRow(info.row.index)}
        />
      ),
    },
    // cells are read by column position, not by column name: a result can
    // contain several columns with the same name (e.g. `select a.id, b.id`).
    // They are rendered from the raw values, so only for the rows shown.
    ...headers.map(
      (header, hIndex): ColumnDef<DataTableFeatures, string[]> => ({
        id: String(hIndex),
        accessorFn: (row) => row[hIndex],
        header,
        size: sizes[hIndex],
        cell: (info) =>
          getColCellContent(info.getValue() as React.ReactNode, sizes[hIndex]),
      }),
    ),
  ];

  return columns;
};

const ResultTable: React.FC<ResultTableProps> = ({ headers, rows }) => {
  const [openRowIndex, setOpenRowIndex] = useState<number | null>(null);
  const columns = useMemo(
    () => getTableConfig(headers, rows, setOpenRowIndex),
    [headers, rows],
  );

  const table = useTable({
    features: dataTableFeatures,
    data: rows,
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
                  style={{ width: header.getSize(), fontWeight: 'bold' }}
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
      {openRowIndex !== null && rows[openRowIndex] && (
        <ResultRowDialog
          headers={headers}
          row={rows[openRowIndex]}
          onClose={() => setOpenRowIndex(null)}
        />
      )}
    </div>
  );
};

export default ResultTable;
