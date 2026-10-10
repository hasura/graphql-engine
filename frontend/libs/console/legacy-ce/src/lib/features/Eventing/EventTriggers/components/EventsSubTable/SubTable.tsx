import React from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import ExpanderButton from '../../../components/ExpanderButton';
import {
  coreTableFeatures,
  CoreTableFeatures,
  Table,
  Text,
} from '@hasura/shared/ui';
import InvocationLogDetails from '../../../components/InvocationLogDetails';
import { Flex } from '@radix-ui/themes';

interface Props {
  rows: any[];
  webhookConf?: string;
}

const invocationColumns = ['http_status', 'id', 'created_at'];

const SubTable: React.FC<Props> = ({ rows, webhookConf }) => {
  const [expandedRowIndex, setExpandedRowIndex] = React.useState<number | null>(
    null,
  );

  const tableColumns = React.useMemo(() => {
    const cols: ColumnDef<
      CoreTableFeatures,
      Record<string, React.ReactNode>
    >[] = [
      {
        id: 'expander',
        header: '',
        size: 40,
        cell: ({ row }) => (
          <ExpanderButton
            isExpanded={expandedRowIndex === row.index}
            onClick={() => {
              setExpandedRowIndex(
                expandedRowIndex === row.index ? null : row.index,
              );
            }}
          />
        ),
      },
      ...invocationColumns.map(
        (
          column,
        ): ColumnDef<CoreTableFeatures, Record<string, React.ReactNode>> => ({
          id: column,
          accessorKey: column,
          header: () => column,
          cell: (info) => <div>{info.getValue() as React.ReactNode}</div>,
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
    <div>
      {webhookConf && (
        <Flex className="row mb-2">
          <Text as="p" className="w-1/6" weight="medium">
            Webhook:
          </Text>
          <Text as="p">{webhookConf}</Text>
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
                    <Table.RowHeaderCell
                      key={header.id}
                      align="center"
                      justify="center"
                    >
                      {header.isPlaceholder ? null : (
                        <Text weight="medium">
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
                const invocationLog = rows[row.index];

                return (
                  <React.Fragment key={row.id}>
                    <Table.Row>
                      {row.getVisibleCells().map((cell) => (
                        <Table.Cell
                          key={cell.id}
                          justify="center"
                          align="center"
                        >
                          {flexRender(
                            cell.column.columnDef.cell,
                            cell.getContext(),
                          )}
                        </Table.Cell>
                      ))}
                    </Table.Row>
                    {isExpanded && (
                      <Table.Row>
                        <Table.Cell colSpan={row.getVisibleCells().length}>
                          <InvocationLogDetails
                            requestPayload={invocationLog.request}
                            responsePayload={invocationLog.response}
                          />
                        </Table.Cell>
                      </Table.Row>
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
      <br />
      <br />
    </div>
  );
};

export default SubTable;
