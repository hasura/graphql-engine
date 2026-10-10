import React from 'react';
import { ColumnDef, flexRender, useTable } from '@tanstack/react-table';
import { FaSort, FaSortUp, FaSortDown } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import {
  PaginationOffset,
  resizableTableFeatures,
  ResizableTableFeatures,
  Table,
} from '@hasura/shared/ui';
import { FilterTableProps } from '../../types';
import ExpanderButton from '../../../components/ExpanderButton';
import { convertDateTimeToLocale } from '@hasura/shared/utils';
import {
  getEventStatusIcon,
  getEventDeliveryIcon,
} from '../../components/utils';
import { OrderBy } from '@hasura/shared/types';
import CancelEventButton from '../../../components/EventsTable/CancelEventButton';

interface Props extends FilterTableProps {
  onCancelEvent?: (
    id: string,
    scheduled_at: string | Date | number,
    onSuccess?: () => void,
  ) => void;
  sortable?: boolean;
  renderExpand: (row: any) => React.ReactNode;
}

const EventsTable: React.FC<Props> = (props) => {
  const {
    rows,
    paginationState,
    setPaginationState,
    columns,
    onCancelEvent,
    sortable,
    renderExpand,
  } = props;
  const [expandedRowIndex, setExpandedRowIndex] = React.useState<number | null>(
    null,
  );

  const sortByColumn = (col: string) => {
    const existingColSort = paginationState.sorts.find((s) => s.column === col);
    const newSort: OrderBy =
      existingColSort && existingColSort.type === 'asc'
        ? {
            column: col,
            type: 'desc',
          }
        : {
            column: col,
            type: 'asc',
          };

    setPaginationState({
      ...paginationState,
      sorts: [newSort],
    });
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

  const onCancelHandler = (
    id: string,
    scheduled_time: string | Date | number,
  ) => {
    if (onCancelEvent) {
      onCancelEvent(id, scheduled_time);
    }
  };

  const rowsFormatted = rows.map((row) => ({
    ...row,
    delivered: getEventDeliveryIcon(row.delivered),
    status: getEventStatusIcon(row.status),
    scheduled_time: row.scheduled_time
      ? convertDateTimeToLocale(row.scheduled_time)
      : undefined,
    created_at: row.created_at
      ? convertDateTimeToLocale(row.created_at)
      : undefined,
  }));

  const tableColumns = React.useMemo(() => {
    const cols: ColumnDef<
      ResizableTableFeatures,
      (typeof rowsFormatted)[number]
    >[] = [
      {
        id: 'expander',
        header: '',
        size: 32,
        enableResizing: false,
        cell: ({ row }) => {
          const isExpanded = expandedRowIndex === row.index;
          return (
            <Flex align="center">
              {columns.includes('actions') && (
                <CancelEventButton
                  id={row.original.id}
                  onClickHandler={() => {
                    onCancelHandler(
                      row.original.id,
                      row.original.scheduled_time,
                    );
                  }}
                />
              )}
              <ExpanderButton
                isExpanded={isExpanded}
                onClick={() =>
                  setExpandedRowIndex(isExpanded ? null : row.index)
                }
              />
            </Flex>
          );
        },
      },
      ...columns
        .filter((column) => column !== 'actions')
        .map(
          (
            column,
          ): ColumnDef<
            ResizableTableFeatures,
            (typeof rowsFormatted)[number]
          > => {
            const existingSort = paginationState.sorts.find(
              (s) => s.column === column,
            );
            return {
              id: column,
              accessorKey: column,
              header: () =>
                sortable ? (
                  <div
                    style={{
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'space-between',
                      width: '100%',
                    }}
                  >
                    <span>{column}</span>
                    {existingSort ? (
                      existingSort.type === 'asc' ? (
                        <FaSortUp
                          style={{ marginLeft: '5px', fontSize: '16px' }}
                        />
                      ) : (
                        <FaSortDown
                          style={{ marginLeft: '5px', fontSize: '16px' }}
                        />
                      )
                    ) : (
                      <FaSort
                        style={{
                          marginLeft: '5px',
                          opacity: 0.5,
                          fontSize: '16px',
                        }}
                      />
                    )}
                  </div>
                ) : (
                  column
                ),
              cell: (info) => <div>{info.getValue() as React.ReactNode}</div>,
            };
          },
        ),
    ];
    return cols;
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [columns, paginationState.sorts, expandedRowIndex]);

  const table = useTable({
    features: resizableTableFeatures,
    data: rowsFormatted,
    columns: tableColumns,
    columnResizeMode: 'onChange',
  });

  return (
    <div data-test="events-table">
      <Table.Root>
        <Table.Header>
          {table.getHeaderGroups().map((headerGroup) => (
            <Table.Row key={headerGroup.id}>
              {headerGroup.headers.map((header) => (
                <Table.RowHeaderCell key={header.id}>
                  <div
                    style={{
                      width: header.getSize(),
                      position: 'relative',
                      cursor:
                        sortable &&
                        header.column.id !== 'expander' &&
                        header.column.id !== 'actions'
                          ? 'pointer'
                          : undefined,
                    }}
                    onClick={() => {
                      if (
                        sortable &&
                        header.column.id !== 'expander' &&
                        header.column.id !== 'actions'
                      ) {
                        sortByColumn(header.column.id);
                      }
                    }}
                  >
                    {header.isPlaceholder
                      ? null
                      : flexRender(
                          header.column.columnDef.header,
                          header.getContext(),
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
              ))}
            </Table.Row>
          ))}
        </Table.Header>
        <Table.Body>
          {table.getRowModel().rows.map((row) => {
            const isExpanded = expandedRowIndex === row.index;
            const currentRow = rows[row.index];
            return (
              <React.Fragment key={row.id}>
                <Table.Row>
                  {row.getVisibleCells().map((cell) => (
                    <Table.Cell
                      key={cell.id}
                      style={{ width: cell.column.getSize() }}
                    >
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext(),
                      )}
                    </Table.Cell>
                  ))}
                </Table.Row>
                {isExpanded && (
                  <tr>
                    <td colSpan={row.getVisibleCells().length} className="p-0">
                      {renderExpand(currentRow)}
                    </td>
                  </tr>
                )}
              </React.Fragment>
            );
          })}
        </Table.Body>
      </Table.Root>
      <PaginationOffset
        offset={paginationState.offset}
        limit={paginationState.limit}
        changePage={changePage}
        changePageSize={changePageSize}
        rows={rows}
      />
    </div>
  );
};
export default EventsTable;
