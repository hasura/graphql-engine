import {
  columnResizingFeature,
  columnSizingFeature,
  columnVisibilityFeature,
  createPaginatedRowModel,
  rowPaginationFeature,
  rowSelectionFeature,
  rowSortingFeature,
  tableFeatures,
} from '@tanstack/react-table';

export const dataTableFeatures = tableFeatures({
  columnVisibilityFeature,
  columnSizingFeature,
  rowPaginationFeature,
  paginatedRowModel: createPaginatedRowModel(),
});

export type DataTableFeatures = typeof dataTableFeatures;

// columnVisibilityFeature and columnSizingFeature are the baseline for every
// table in this app: row.getVisibleCells() (used by every renderer, directly
// or via CardedTableFromReactTable/ReactTableWrapper) requires
// columnVisibilityFeature, and setting a column's `size` or reading
// header/column.getSize() requires columnSizingFeature.
export const coreTableFeatures = tableFeatures({
  columnVisibilityFeature,
  columnSizingFeature,
});
export type CoreTableFeatures = typeof coreTableFeatures;

// Interactive column resizing (drag handles), on top of the baseline.
export const resizableTableFeatures = tableFeatures({
  columnVisibilityFeature,
  columnSizingFeature,
  columnResizingFeature,
});
export type ResizableTableFeatures = typeof resizableTableFeatures;

// Sorting state here is controlled/manual (fed into a server-side GraphQL
// order_by, not a client-side sorted row model), so only rowSortingFeature is
// registered — no sortedRowModel/sortFns.
export const sortableSelectableTableFeatures = tableFeatures({
  columnVisibilityFeature,
  columnSizingFeature,
  rowSortingFeature,
  rowSelectionFeature,
});
export type SortableSelectableTableFeatures =
  typeof sortableSelectableTableFeatures;
