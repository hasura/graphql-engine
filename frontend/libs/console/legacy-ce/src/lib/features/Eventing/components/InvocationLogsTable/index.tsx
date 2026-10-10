import React from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import type { FilterTableProps } from '../../EventTriggers/types';
import { QualifiedDataSource, QualifiedTable } from '@hasura/shared/types';
import { useRedeliverEvent } from '../../EventTriggers/hooks/useRedeliverEvent';
import {
  Dialog,
  PaginationOffset,
  resizableTableFeatures,
  ResizableTableFeatures,
  Table,
  Text,
} from '@hasura/shared/ui';
import { useErrorNotification } from '@hasura/metadata/api';
import RedeliverEvent from './RedeliverEvent';
import { getInvocationLogStatus } from '../../EventTriggers/components/utils';
import { convertDateTimeToLocale } from '@hasura/shared/utils';
import RedeliverEventButton from './RedeliverEventButton';
import ExpanderButton from '../ExpanderButton';
import { Flex } from '@radix-ui/themes';
import InvocationLogDetails from '../InvocationLogDetails';

interface Props extends FilterTableProps {
  tableDef?: QualifiedTable;
  source?: QualifiedDataSource;
}

const InvocationLogsTable: React.FC<Props> = ({
  rows,
  paginationState,
  setPaginationState,
  columns,
  tableDef,
  source,
}) => {
  const [redeliveredEventId, setRedeliveredEventId] = React.useState<
    string | null
  >(null);
  const [isRedelivering, setIsRedelivering] = React.useState(false);
  const [expandedRowIndex, setExpandedRowIndex] = React.useState<number | null>(
    null,
  );
  const redeliverEvent = useRedeliverEvent();
  const showErrorNotification = useErrorNotification();

  const redeliverHandler = (eventId: string) => {
    if (!source) {
      return;
    }

    setIsRedelivering(true);
    return redeliverEvent({
      eventId,
      sourceKind: source.kind,
    })
      .then(() => {
        setRedeliveredEventId(eventId);
      })
      .catch((error) => {
        showErrorNotification({
          title: 'Failed to redeliver event',
          error,
        });
      })
      .finally(() => {
        setIsRedelivering(false);
      });
  };

  const redeliverModal = (eventId: string) => {
    if (!redeliveredEventId || !source?.kind) return null;

    if (eventId !== redeliveredEventId) {
      return null;
    }

    return (
      <Dialog
        title="Redeliver Event"
        size="xxxl"
        onClose={() => setRedeliveredEventId(null)}
      >
        <RedeliverEvent eventId={redeliveredEventId} source={source} />
      </Dialog>
    );
  };
  const sortByColumn = (col: string) => {
    // Remove all the existing order_bys
    const existingColSort = paginationState.sorts.find((s) => s.column === col);
    if (existingColSort && existingColSort.type === 'asc') {
      setPaginationState({
        ...paginationState,
        sorts: [
          {
            column: col,
            type: 'desc',
          },
        ],
      });
    } else {
      setPaginationState({
        ...paginationState,
        sorts: [
          {
            column: col,
            type: 'asc',
          },
        ],
      });
    }
  };
  const changePage = (page: number) => {
    if (paginationState.offset !== page * paginationState.limit) {
      setPaginationState({
        ...paginationState,
        offset: page * paginationState.limit,
      });
    }
  };
  const changePageSize = (size: number) => {
    if (paginationState.limit !== size) {
      setPaginationState({
        ...paginationState,
        limit: size,
      });
    }
  };

  const rowsFormatted = rows.map((r) => {
    const formattedRow: Record<string, unknown> = {};
    Object.keys(r).forEach((col: string) => {
      formattedRow[col] = r[col];
    });

    return {
      ...formattedRow,
      status: (
        <Flex className="h-full" align="center" justify="center">
          {getInvocationLogStatus(r.http_status || r.status)}
        </Flex>
      ),
      created_at: r.created_at && convertDateTimeToLocale(r.created_at),
      id: (
        <div>
          {r.id}
          {columns.includes('redeliver') ? redeliverModal(r.event_id) : null}
        </div>
      ),
    };
  });

  const tableColumns = React.useMemo(() => {
    const cols: ColumnDef<
      ResizableTableFeatures,
      (typeof rowsFormatted)[number]
    >[] = [
      {
        id: 'expander',
        header: '',
        size: 40,
        cell: ({ row }) => {
          const isExpanded = expandedRowIndex === row.index;
          const dataRow = rows[row.index];
          return (
            <Flex align="center" gap="2">
              {columns.includes('redeliver') && (
                <RedeliverEventButton
                  onClickHandler={(e) => {
                    if (isRedelivering) {
                      return;
                    }

                    e.stopPropagation();

                    redeliverHandler(dataRow.event_id);
                  }}
                />
              )}
              <ExpanderButton
                isExpanded={isExpanded}
                onClick={() => {
                  setExpandedRowIndex(isExpanded ? null : row.index);
                }}
              />
            </Flex>
          );
        },
      },
      ...columns
        .filter((column) => column !== 'redeliver')
        .map(
          (
            column,
          ): ColumnDef<
            ResizableTableFeatures,
            (typeof rowsFormatted)[number]
          > => ({
            id: column,
            accessorKey: column,
            header: column,
            size: column === 'status' ? 40 : undefined,
            cell: (info) => info.getValue(),
          }),
        ),
    ];
    return cols;
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [expandedRowIndex, isRedelivering, columns, tableDef, source]);

  const table = useTable({
    features: resizableTableFeatures,
    data: rowsFormatted,
    columns: tableColumns,
    columnResizeMode: 'onChange',
  });

  return (
    <div>
      <Table.Root variant="surface">
        <Table.Header>
          {table.getHeaderGroups().map((headerGroup) => (
            <Table.Row key={headerGroup.id}>
              {headerGroup.headers.map((header) => {
                const columnWidth = header.getSize();

                return (
                  <Table.RowHeaderCell
                    key={header.id}
                    align="center"
                    justify="center"
                    width={`${columnWidth}px`}
                  >
                    <div
                      style={{
                        display: 'inline-block',
                        position: 'relative',
                        cursor:
                          header.column.id !== 'expander' &&
                          header.column.id !== 'Actions'
                            ? 'pointer'
                            : undefined,
                      }}
                      onClick={() => {
                        if (
                          header.column.id !== 'expander' &&
                          header.column.id !== 'Actions'
                        ) {
                          sortByColumn(header.column.id);
                        }
                      }}
                    >
                      {header.isPlaceholder ? null : (
                        <Text weight="medium">
                          {flexRender(
                            header.column.columnDef.header,
                            header.getContext(),
                          )}
                        </Text>
                      )}
                      {header.column.getCanResize() && (
                        <div
                          onMouseDown={header.getResizeHandler()}
                          onTouchStart={header.getResizeHandler()}
                          onClick={(e) => e.stopPropagation()}
                          style={{
                            position: 'absolute',
                            right: 0,
                            top: 0,
                            height: '100%',
                            width: '5px',
                            cursor: 'col-resize',
                            userSelect: 'none',
                            touchAction: 'none',
                          }}
                        />
                      )}
                    </div>
                  </Table.RowHeaderCell>
                );
              })}
            </Table.Row>
          ))}
        </Table.Header>
        <Table.Body>
          {table.getRowModel().rows.map((row) => {
            const isExpanded = expandedRowIndex === row.index;
            const finalRow = rows[row.index];

            return (
              <React.Fragment key={row.id}>
                <Table.Row>
                  {row.getVisibleCells().map((cell) => {
                    const columnSize = cell.column.getSize();

                    return (
                      <Table.Cell
                        key={cell.id}
                        width={columnSize ? `${columnSize}px` : undefined}
                      >
                        <Flex
                          className="w-full h-full"
                          align="center"
                          justify="center"
                        >
                          {flexRender(
                            cell.column.columnDef.cell,
                            cell.getContext(),
                          )}
                        </Flex>
                      </Table.Cell>
                    );
                  })}
                </Table.Row>
                {isExpanded && (
                  <Table.Row>
                    <Table.Cell colSpan={row.getVisibleCells().length}>
                      <InvocationLogDetails
                        requestPayload={finalRow?.request ?? {}}
                        responsePayload={finalRow?.response ?? {}}
                      />
                    </Table.Cell>
                  </Table.Row>
                )}
              </React.Fragment>
            );
          })}
        </Table.Body>
      </Table.Root>
      <PaginationOffset
        className="mt-4"
        justify="center"
        offset={paginationState.offset}
        limit={paginationState.limit}
        changePage={changePage}
        changePageSize={changePageSize}
        rows={rows}
      />
    </div>
  );
};
export default InvocationLogsTable;
