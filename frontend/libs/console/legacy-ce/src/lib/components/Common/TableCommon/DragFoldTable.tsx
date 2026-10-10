import React from 'react';
import {
  ColumnDef,
  RowData,
  flexRender,
  useTable,
} from '@tanstack/react-table';
import { FaCaretLeft, FaCaretRight } from 'react-icons/fa';
import {
  resizableTableFeatures,
  ResizableTableFeatures,
  Table,
} from '@hasura/shared/ui';
import { Theme } from '@radix-ui/themes';
export * from './utils';

export type DragFoldColumnDef<TData extends RowData> = ColumnDef<
  ResizableTableFeatures,
  TData
> & {
  id: string;
  foldable?: boolean;
  size?: number;
};

type ReorderEntry = { newOrder: number; defaultOrder: number };

const FOLDED_SIZE = 22;

const applyReorders = <TData extends RowData>(
  columns: DragFoldColumnDef<TData>[],
  reorders: ReorderEntry[],
) => {
  const reordered = [...columns];
  reorders.forEach((o) => {
    reordered.splice(o.newOrder, 0, reordered.splice(o.defaultOrder, 1)[0]);
  });
  return reordered;
};

export type DragFoldTableProps<TData extends RowData> = {
  data: TData[];
  columns: DragFoldColumnDef<TData>[];
  headerTitle?: string;
  onSort?: (columnId: string, clearExisting: boolean) => void;
  getSortIcon?: (columnId: string) => React.ReactNode;
  defaultCollapsed?: Record<string, boolean>;
  onCollapseChange?: (collapsed: Record<string, boolean>) => void;
  defaultReorders?: ReorderEntry[];
  onOrderChange?: (reorders: ReorderEntry[]) => void;
  renderPagination?: () => React.ReactNode;
};

const DragFoldTable = <TData extends RowData>({
  data,
  columns,
  headerTitle,
  onSort,
  defaultCollapsed,
  onCollapseChange,
  defaultReorders,
  onOrderChange,
  renderPagination,
}: DragFoldTableProps<TData>) => {
  const [collapsed, setCollapsed] = React.useState<Record<string, boolean>>(
    defaultCollapsed || {},
  );
  const [reorders, setReorders] = React.useState<ReorderEntry[]>(
    defaultReorders || [],
  );
  const draggedRef = React.useRef<number | null>(null);

  const toggleCollapsed = (columnId: string) => {
    const newCollapsed = { ...collapsed, [columnId]: !collapsed[columnId] };
    setCollapsed(newCollapsed);
    onCollapseChange?.(newCollapsed);
  };

  const orderedColumns = React.useMemo(
    () => applyReorders(columns, reorders),
    [columns, reorders],
  );

  const tableColumns = React.useMemo<
    ColumnDef<ResizableTableFeatures, TData>[]
  >(
    () =>
      orderedColumns.map((col) => {
        const isFolded = col.foldable && collapsed[col.id];
        if (!isFolded) return col;
        return {
          ...col,
          size: FOLDED_SIZE,
          cell: () => '',
        };
      }),
    [orderedColumns, collapsed],
  );

  const table = useTable({
    features: resizableTableFeatures,
    data,
    columns: tableColumns,
    columnResizeMode: 'onChange',
  });

  const handleHeaderDrop = (dropIndex: number) => {
    if (draggedRef.current === null) return;
    const newReorders = [
      ...reorders,
      { newOrder: dropIndex, defaultOrder: draggedRef.current },
    ];
    setReorders(newReorders);
    onOrderChange?.(newReorders);
  };

  return (
    <div className="esr-table">
      {renderPagination?.()}
      <Theme appearance="light">
        <Table.Root variant="surface">
          <Table.Header>
            {table.getHeaderGroups().map((headerGroup) => (
              <Table.Row key={headerGroup.id}>
                {headerGroup.headers.map((header, headerIndex) => {
                  const columnDef = header.column
                    .columnDef as DragFoldColumnDef<TData>;
                  const isFoldable = Boolean(columnDef.foldable);
                  const isFolded = isFoldable && collapsed[header.column.id];

                  return (
                    <Table.RowHeaderCell key={header.id}>
                      <div
                        className="draggable-header"
                        title={headerTitle || 'Drag to rearrange'}
                        draggable={isFoldable}
                        onDragStart={(e) => {
                          e.stopPropagation();
                          draggedRef.current = headerIndex;
                        }}
                        onDragOver={(e) => e.preventDefault()}
                        onDrop={(e) => {
                          e.preventDefault();
                          handleHeaderDrop(headerIndex);
                        }}
                        style={{
                          width: header.getSize(),
                          position: 'relative',
                          display: 'flex',
                          alignItems: 'center',
                          cursor: isFoldable ? 'pointer' : undefined,
                        }}
                        onClick={() => {
                          if (!isFoldable && onSort && header.column.id) {
                            onSort(header.column.id, true);
                          }
                        }}
                      >
                        {isFoldable && (
                          <span
                            style={{
                              cursor: 'pointer',
                              fontSize: '14px',
                              marginRight: isFolded ? 0 : '5px',
                            }}
                            onClick={(e) => {
                              e.stopPropagation();
                              toggleCollapsed(header.column.id);
                            }}
                          >
                            {isFolded ? (
                              <FaCaretRight title="Expand column" />
                            ) : (
                              <FaCaretLeft title="Collapse column" />
                            )}
                          </span>
                        )}
                        {!isFolded &&
                          (header.isPlaceholder
                            ? null
                            : flexRender(
                                header.column.columnDef.header,
                                header.getContext(),
                              ))}
                        {!isFolded && header.column.getCanResize() && (
                          <div
                            onMouseDown={(e) => {
                              e.stopPropagation();
                              header.getResizeHandler()(e);
                            }}
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
      </Theme>
    </div>
  );
};

export default DragFoldTable;
