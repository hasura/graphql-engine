import React from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import {
  coreTableFeatures,
  CoreTableFeatures,
  Table,
  Text,
} from '@hasura/shared/ui';
import ExpanderButton from '../../../components/ExpanderButton';
import InvocationLogDetails from '../../../components/InvocationLogDetails';
import { Flex } from '@radix-ui/themes';
import { ScheduledEventInvocation } from '@hasura/shared/types';

interface Props {
  rows: ScheduledEventInvocation[];
  webhookConf?: any;
  comment?: string;
}

const invocationColumns = ['status', 'id', 'created_at'];

const EventSubTable: React.FC<Props> = ({ rows, webhookConf, comment }) => {
  const [expandedRowIndex, setExpandedRowIndex] = React.useState<number | null>(
    null,
  );

  const tableColumns = React.useMemo(() => {
    const cols: ColumnDef<CoreTableFeatures, Record<string, unknown>>[] = [
      {
        id: 'expander',
        header: '',
        size: 40,
        cell: ({ row }) => (
          <ExpanderButton
            isExpanded={expandedRowIndex === row.index}
            onClick={() =>
              setExpandedRowIndex(
                expandedRowIndex === row.index ? null : row.index,
              )
            }
          />
        ),
      },
      ...invocationColumns.map(
        (column): ColumnDef<CoreTableFeatures, Record<string, unknown>> => ({
          id: column,
          accessorKey: column,
          header: () => column,
          cell: (info) => info.getValue(),
        }),
      ),
    ];
    return cols;
  }, [expandedRowIndex]);

  const table = useTable({
    features: coreTableFeatures,
    data: rows,
    columns: tableColumns,
  });

  return (
    <div className="p-4">
      {webhookConf && (
        <Flex className="row mb-2">
          <Text as="p" className="w-1/6" weight="medium">
            Webhook:
          </Text>
          <Text as="p">{webhookConf}</Text>
        </Flex>
      )}
      {comment && (
        <Flex className="row mb-2">
          <Text as="p" className="w-1/6" weight="medium">
            Comment:
          </Text>
          <Text as="p">{comment}</Text>
        </Flex>
      )}
      <div className="mb-2">
        <Text weight="medium">Recent Invocations:</Text>
      </div>
      <div>
        {rows.length ? (
          <Table.Root>
            <Table.Header>
              {table.getHeaderGroups().map((headerGroup) => (
                <Table.Row key={headerGroup.id}>
                  {headerGroup.headers.map((header) => (
                    <Table.RowHeaderCell key={header.id}>
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
              {table.getRowModel().rows.map((row) => {
                const isExpanded = expandedRowIndex === row.index;
                const invocationLog = rows[row.index];

                return (
                  <React.Fragment key={row.id}>
                    <Table.Row>
                      {row.getVisibleCells().map((cell) => (
                        <Table.Cell key={cell.id}>
                          {flexRender(
                            cell.column.columnDef.cell,
                            cell.getContext(),
                          )}
                        </Table.Cell>
                      ))}
                    </Table.Row>
                    {isExpanded && (
                      <tr>
                        <td
                          colSpan={row.getVisibleCells().length}
                          className="p-0"
                        >
                          <InvocationLogDetails
                            requestPayload={invocationLog.request}
                            responsePayload={invocationLog.response}
                          />
                        </td>
                      </tr>
                    )}
                  </React.Fragment>
                );
              })}
            </Table.Body>
          </Table.Root>
        ) : (
          <Text>No data available</Text>
        )}
      </div>
    </div>
  );
};

export default EventSubTable;
