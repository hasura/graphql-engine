import React from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import { EventInvocation } from '@hasura/shared/types';
import ExpanderButton from '../ExpanderButton';
import {
  coreTableFeatures,
  CoreTableFeatures,
  Table,
  Text,
} from '@hasura/shared/ui';
import InvocationLogDetails from '../InvocationLogDetails';

type RedeliverEventLogsTableProps = {
  className?: string;
  logs: EventInvocation[];
  invocationColumns: string[];
  expandedRowIndex: number | null;
  setExpandedRowIndex: (index: number | null) => void;
};

const RedeliverEventLogsTable: React.FC<RedeliverEventLogsTableProps> = ({
  className,
  logs,
  invocationColumns,
  expandedRowIndex,
  setExpandedRowIndex,
}) => {
  const tableColumns = React.useMemo(() => {
    const cols: ColumnDef<CoreTableFeatures, EventInvocation>[] = [
      {
        id: 'expander',
        header: '',
        size: 40,
        cell: ({ row }) => (
          <ExpanderButton
            isExpanded={expandedRowIndex === row.index}
            onClick={() => {
              {
                setExpandedRowIndex(
                  expandedRowIndex === row.index ? null : row.index,
                );
              }
            }}
          />
        ),
      },
      ...invocationColumns.map(
        (column): ColumnDef<CoreTableFeatures, EventInvocation> => ({
          id: column,
          accessorKey: column,
          header: column,
          cell: (info) => <div>{info.getValue() as React.ReactNode}</div>,
        }),
      ),
    ];
    return cols;
  }, [invocationColumns, expandedRowIndex]);

  const table = useTable({
    features: coreTableFeatures,
    data: logs,
    columns: tableColumns,
  });

  return (
    <Table.Root className={className}>
      <Table.Header>
        {table.getHeaderGroups().map((headerGroup) => (
          <Table.Row key={headerGroup.id}>
            {headerGroup.headers.map((header) => (
              <Table.RowHeaderCell key={header.id} align="center">
                {header.isPlaceholder ? null : (
                  <Text weight="medium" align="center">
                    {flexRender(
                      header.column.columnDef.header,
                      header.getContext(),
                    )}
                  </Text>
                )}
              </Table.RowHeaderCell>
            ))}
          </Table.Row>
        ))}
      </Table.Header>
      <Table.Body>
        {table.getRowModel().rows.map((row) => {
          const isExpanded = expandedRowIndex === row.index;
          const finalRow = logs[row.index];
          const currentPayload = finalRow?.request
            ? JSON.stringify(finalRow.request, null, 4)
            : '';
          const finalResponse = finalRow?.response
            ? JSON.stringify(finalRow.response, null, 4)
            : '';

          return (
            <React.Fragment key={row.id}>
              <Table.Row>
                {row.getVisibleCells().map((cell) => (
                  <Table.Cell key={cell.id} align="center">
                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                  </Table.Cell>
                ))}
              </Table.Row>
              {isExpanded && (
                <Table.Row>
                  <Table.Cell colSpan={row.getVisibleCells().length}>
                    <InvocationLogDetails
                      requestPayload={currentPayload}
                      responsePayload={finalResponse}
                    />
                  </Table.Cell>
                </Table.Row>
              )}
            </React.Fragment>
          );
        })}
      </Table.Body>
    </Table.Root>
  );
};

export default RedeliverEventLogsTable;
