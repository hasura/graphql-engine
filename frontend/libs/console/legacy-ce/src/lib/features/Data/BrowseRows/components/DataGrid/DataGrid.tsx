import {
  Relationship,
  useListAllDatabaseRelationships,
} from '../../../../DatabaseRelationships';
import {
  ExportFileFormat,
  Operator,
  TableRow,
  useDeleteRows,
  useExportRows,
  useColumnOperators,
  useEnumColumnValues,
  useRows,
  useTableColumns,
} from '@hasura/metadata/data-source';
import uniqBy from 'lodash/uniqBy';
import { Source, Table, WhereClause, OrderBy } from '@hasura/shared/types';
import {
  ErrorCard,
  SkeletonList,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { ColumnSort, RowSelectionState } from '@tanstack/react-table';
import React, { useEffect, useState } from 'react';
import {
  DEFAULT_ORDER_BY_CLAUSES,
  DEFAULT_PAGE_INDEX,
  DEFAULT_PAGE_SIZE,
  DEFAULT_QUERY_DIALOG_STATE,
  DEFAULT_SORT_CLAUSES,
  DEFAULT_WHERE_CLAUSES,
} from './constants';
import { DataTableOptions } from './parts/DataTableOptions';
import { ReactTableWrapper } from './parts/ReactTableWrapper';
import { QueryDialog } from './QueryDialog';
import { transformToOrderByClause } from './utils';
import { adaptSelectedRowIdsToWhereClause } from './DataGrid.utils';

export type DataGridOptions = {
  where?: WhereClause[];
  offset?: number;
  limit?: number;
  order_by?: OrderBy[];
};

export type OnRelationshipOpenProps = {
  table: Table;
  options?: DataGridOptions;
  relationshipName: string;
  parentOptions?: DataGridOptions;
  parentTableFilter: WhereClause[];
};

export interface DataGridProps {
  table: Table;
  source: Source;
  onRelationshipOpen?: (props: OnRelationshipOpenProps) => void;
  onRelationshipClose?: (relationshipName: string) => void;
  options?: DataGridOptions;
  activeRelationships?: string[];
  disableRunQuery?: boolean;
  updateOptions?: (options: DataGridOptions) => void;
}

const getEqualToOperator = (operators: Operator[]) => {
  const equalToOperator = operators.find((op) => op.name === 'equals');
  return equalToOperator?.value ?? '_eq';
};

export const DataGrid = (props: DataGridProps) => {
  const {
    source,
    table,
    options,
    onRelationshipOpen,
    disableRunQuery,
    activeRelationships,
    onRelationshipClose,
    updateOptions,
  } = props;

  /**
   * States used by the component - passed around and shared by the table and it's options
   */
  const [pageIndex, setPageIndex] = useState(
    disableRunQuery
      ? DEFAULT_PAGE_INDEX
      : (options?.offset ?? DEFAULT_PAGE_INDEX) /
          (options?.limit ?? DEFAULT_PAGE_SIZE),
  );

  const [pageSize, setPageSize] = useState(options?.limit ?? DEFAULT_PAGE_SIZE);
  const [sorting, setSorting] =
    React.useState<ColumnSort[]>(DEFAULT_SORT_CLAUSES);
  const [queryDialogVisibility, setQueryDialogVisibility] = useState(
    DEFAULT_QUERY_DIALOG_STATE,
  );
  const [whereClauses, setWhereClauses] = React.useState<WhereClause[]>(
    options?.where ?? DEFAULT_WHERE_CLAUSES,
  );
  const [orderByClauses, setOrderClauses] = React.useState<OrderBy[]>(
    options?.order_by ?? DEFAULT_ORDER_BY_CLAUSES,
  );

  /**
   * Function to reset everything back to the original state
   */
  const refreshAllOptions = () => {
    setPageIndex(DEFAULT_PAGE_INDEX);
    setPageSize(DEFAULT_PAGE_SIZE);
    setSorting(DEFAULT_SORT_CLAUSES);
    setWhereClauses(DEFAULT_WHERE_CLAUSES);
    setOrderClauses(DEFAULT_ORDER_BY_CLAUSES);
    updateOptions?.({
      limit: pageSize,
      offset: pageIndex * pageSize,
      where: DEFAULT_WHERE_CLAUSES,
      order_by: DEFAULT_ORDER_BY_CLAUSES,
    });
  };

  const rowOptions = {
    limit: pageSize,
    offset: pageIndex * pageSize,
    order_by: [...orderByClauses, ...transformToOrderByClause(sorting)],
    where: { _and: whereClauses },
  };

  const { data: tableColumnQueryResult, isPending: isColumnsPending } =
    useTableColumns({
      table,
      source,
    });
  // filters only offer the operators the column type supports
  const { data: columnOperators } = useColumnOperators({ source, table });
  const { data: enumColumnValues } = useEnumColumnValues({ source, table });
  const allOperators = uniqBy(
    [
      ...(tableColumnQueryResult?.supportedOperators ?? []),
      ...Object.values(columnOperators ?? {}).flat(),
    ],
    (operator) => operator.value,
  );

  // rows are keyed by the table column names
  const primaryKeys =
    tableColumnQueryResult?.columns
      .filter((column) => column.isPrimaryKey)
      .map((column) => column.name) ?? [];

  /**
   * React query hook wrapper to get the rows for
   * the current props.table and props.dataSourceName
   */
  const {
    data: rows,
    isPending: isRowsPending,
    isError,
    error: rowsError,
    refetch: refetchRows,
  } = useRows({
    table,
    source,
    options: rowOptions,
    columns: tableColumnQueryResult?.columns,
  });

  // the rows query waits for the columns, and a disabled query isn't
  // `isLoading` in React Query v5. A table without columns never fetches rows.
  const isRowsLoading =
    isColumnsPending ||
    (isRowsPending && !!tableColumnQueryResult?.columns.length);

  const { mutateAsync: deleteRow, isPending: isDeleting } = useDeleteRows();
  const destructiveConfirm = useDestructiveConfirm();

  const { data: relationships, isFetching } = useListAllDatabaseRelationships({
    table,
    dataSourceName: source.name,
  });

  useEffect(() => {
    if (!disableRunQuery)
      updateOptions?.({
        limit: pageSize,
        offset: pageIndex * pageSize,
        order_by: orderByClauses,
        where: whereClauses,
      });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [pageIndex, pageSize]);

  const { onExportRows } = useExportRows();

  const [selectedRows, setSelectedRows] = useState<RowSelectionState>({});

  const onExportSelectedRows = (exportFileFormat: ExportFileFormat) => {
    const whereClause = adaptSelectedRowIdsToWhereClause({
      rowsId: selectedRows,
      rows,
      primaryKeys,
    });

    return onExportRows(
      {
        columns: tableColumnQueryResult?.columns,
        source,
        options: {
          where: {
            _or: whereClause,
          },
          order_by: orderByClauses,
        },
        table,
      },
      exportFileFormat,
    );
  };

  const onExportAllRows = (exportFileFormat: ExportFileFormat) =>
    onExportRows(
      {
        columns: tableColumnQueryResult?.columns,
        source,
        options: {
          where: { _and: whereClauses },
          order_by: orderByClauses,
        },
        table,
      },
      exportFileFormat,
    );

  const handleOnRelationshipClick = ({
    relationship,
    rowData,
  }: {
    relationship: Relationship;
    rowData: TableRow;
  }) => {
    /**
     * Only local relationship can be accessed via browse rows.
     */
    if (relationship.type !== 'localRelationship') return;

    const equalToOperator = getEqualToOperator(
      tableColumnQueryResult?.supportedOperators ?? [],
    );

    const currentTablePrimaryKeys =
      tableColumnQueryResult?.columns
        .filter((col) => col.isPrimaryKey)
        .map((col) => col.name) ?? [];

    const currentTableFilters: WhereClause[] = Object.entries(rowData)
      .filter(([fromColumn]) => {
        // if primary key is present for the current table then only use that column for the filter
        if (currentTablePrimaryKeys.length) {
          return currentTablePrimaryKeys.includes(fromColumn);
        }

        // in case of databases like BigQuery where there is no concept of PKs, then every column is valid filter
        return true;
      })
      .map(([fromColumn, value]) => {
        return {
          [fromColumn]: {
            [equalToOperator]: value as string | number | boolean,
          },
        };
      });

    /**
     * This will be the set of WhereClauses that will by default when opening the active relationship.
     * To find the columns to be used for the where clauses, we utilise the columnMapping from the relationships definition
     */
    const relationshipTableFilters: WhereClause[] = Object.entries(
      rowData,
    ).reduce<WhereClause[]>((acc, currentEntry) => {
      const [fromColumn, value] = currentEntry;

      const referenceTableColumn = relationship.definition.mapping[fromColumn];

      // If there is no reference column mapping, then keep the original result
      if (!referenceTableColumn) return acc;

      // If there is a reference column mapping, then add it to the filters.
      return [
        ...acc,
        {
          [referenceTableColumn]: {
            [equalToOperator]: value,
          },
        },
      ];
    }, []);

    if (!relationship.definition.toTable) {
      return;
    }

    onRelationshipOpen?.({
      /**
       * the target table is taken from the relationship definition
       */
      table: relationship.definition.toTable,
      /**
       * only whereClause filters are required to narrow down a relationship's rows
       */
      options: {
        where: relationshipTableFilters,
      },
      /**
       * This will be used to generate a unique identifier for the opened relationship view
       */
      relationshipName: relationship.name,
      /**
       * This will be applied on the current table to show only the row that's been clicked on.
       */
      parentTableFilter: currentTableFilters,
      parentOptions: {
        where: currentTableFilters,
        order_by: orderByClauses,
        limit: pageSize,
        offset: pageIndex * pageSize,
      },
    });
  };

  /**
   * Runs a delete once confirmed. The mutation already notifies the error, so
   * a failure only keeps the rows.
   */
  const deleteAfterConfirm = (
    resourceName: string,
    where: Record<string, unknown>,
    onDeleted?: () => void,
  ) => {
    const pks = (tableColumnQueryResult?.columns ?? []).filter(
      (col) => col.isPrimaryKey,
    );

    return destructiveConfirm({
      resourceName,
      resourceType: 'Row',
      destroyTerm: 'delete',
      appendTerm: 'This action cannot be undone.',
      onConfirm: async () => {
        try {
          await deleteRow({ source, table, columns: pks, where });
        } catch {
          return false;
        }
        refetchRows();
        onDeleted?.();
        return true;
      },
    });
  };

  const handleDeleteRow = (row: Record<string, any>) => {
    const pkNames = (tableColumnQueryResult?.columns ?? [])
      .filter((col) => col.isPrimaryKey)
      .map((col) => col.name);
    const where = Object.fromEntries(pkNames.map((name) => [name, row[name]]));

    // e.g. `id: 1, code: "a"`
    const resourceName = pkNames
      .map((name) => `${name}: ${JSON.stringify(row[name])}`)
      .join(', ');

    return deleteAfterConfirm(resourceName, where);
  };

  const handleDeleteRows = () => {
    const whereClause = adaptSelectedRowIdsToWhereClause({
      rowsId: selectedRows,
      rows,
      primaryKeys,
    });

    return deleteAfterConfirm(
      `${whereClause.length} selected ${whereClause.length === 1 ? 'row' : 'rows'}`,
      { _or: whereClause },
      () => setSelectedRows({}),
    );
  };

  return (
    <div>
      <DataTableOptions
        pagination={{
          goToNextPage: () => {
            setPageIndex((currentPage) => currentPage + 1);
          },
          goToPreviousPage: () => {
            setPageIndex((currentPage) => currentPage - 1);
          },
          isNextPageDisabled: (rows ?? []).length < pageSize,
          isPreviousPageDisabled: pageIndex <= 0,
          pageSize,
          setPageSize,
          pageIndex,
        }}
        query={{
          disableRunQuery,
          onQuerySearch: () => {
            setQueryDialogVisibility(true);
          },
          onRefreshQueryOptions: refreshAllOptions,
          orderByClauses,
          whereClauses,
          supportedOperators: allOperators,
          removeWhereClause: (id) => {
            const newWhereClauses = whereClauses.filter((_, i) => i !== id);
            setWhereClauses(whereClauses.filter((_, i) => i !== id));
            updateOptions?.({
              limit: pageSize,
              offset: pageIndex * pageSize,
              where: newWhereClauses,
              order_by: orderByClauses,
            });
          },
          removeOrderByClause: (id) => {
            const newOrderByClauses = orderByClauses.filter((_, i) => i !== id);
            setOrderClauses(newOrderByClauses);
            updateOptions?.({
              limit: pageSize,
              offset: pageIndex * pageSize,
              where: whereClauses,
              order_by: newOrderByClauses,
            });
          },
          onExportRows: onExportAllRows,
          onExportSelectedRows,
          selectedRowsCount: Object.keys(selectedRows).length,
          isDeleting,
          onDeleteRows: handleDeleteRows,
        }}
      />

      {isRowsLoading || isFetching ? (
        <div className="my-4">
          <SkeletonList height="20px" count={8} className="my-2" />
        </div>
      ) : isError ? (
        <div className="my-4">
          <ErrorCard
            headline="Unable to fetch rows"
            error={rowsError}
            collapsible={false}
          />
        </div>
      ) : (
        <ReactTableWrapper
          rows={rows ?? []}
          source={source}
          table={table}
          sort={{
            sorting,
            setSorting,
          }}
          relationships={{
            allRelationships: relationships ?? [],
            activeRelationships,
            onClose: (relationshipName) => {
              onRelationshipClose?.(relationshipName);
            },
            onClick: handleOnRelationshipClick,
          }}
          selectedRows={selectedRows}
          onRowsSelect={setSelectedRows}
          onRowDelete={handleDeleteRow}
          // reload the rows to show the saved row
          onRowEdited={() => refetchRows()}
          isRowsSelectionEnabled={primaryKeys.length > 0}
          tableColumns={tableColumnQueryResult?.columns ?? []}
          disabled={isDeleting}
        />
      )}

      {queryDialogVisibility && (
        <QueryDialog
          onClose={() => {
            setQueryDialogVisibility(false);
          }}
          tableColumns={tableColumnQueryResult}
          columnOperators={columnOperators}
          enumColumnValues={enumColumnValues}
          onSubmit={(data) => {
            const { filters, sorts } = data;
            setWhereClauses(
              filters.map((filter) => {
                return {
                  [filter.column]: {
                    [filter.operator]: filter.value ?? '',
                  },
                };
              }),
            );
            setOrderClauses(sorts);

            if (!disableRunQuery)
              updateOptions?.({
                limit: pageSize,
                offset: pageIndex * pageSize,
                order_by: sorts,
                where: filters.map((filter) => {
                  return {
                    [filter.column]: {
                      [filter.operator]: filter.value ?? '',
                    },
                  };
                }),
              });

            setQueryDialogVisibility(false);
          }}
          filters={whereClauses?.map((clause) => {
            const [column, rest] = Object.entries(clause)[0];
            const [operator, value] = Object.entries(rest)[0];
            return { column, operator, value };
          })}
          sorts={orderByClauses}
        />
      )}
    </div>
  );
};
