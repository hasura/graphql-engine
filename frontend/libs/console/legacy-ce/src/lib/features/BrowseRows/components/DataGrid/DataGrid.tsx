import {
  Relationship,
  useListAllDatabaseRelationships,
} from '../../../DatabaseRelationships';
import {
  Feature,
  Operator,
  OrderBy,
  TableRow,
  WhereClause,
} from '../../../DataSource';
import { Table } from '../../../hasura-metadata-types';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { ColumnSort } from '@tanstack/react-table';
import React, { useEffect, useState } from 'react';
import Skeleton from 'react-loading-skeleton';
import { useDeleteRow } from '../../../DeleteRow';
import { useQueryClient } from 'react-query';
import { useFireNotification } from '../../../../new-components/Notifications';
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
import { useRows, useTableColumns } from '../../hooks';
import { transformToOrderByClause } from './utils';
import { useExportRows } from '../../hooks/useExportRows/useExportRows';
import { adaptSelectedRowIdsToWhereClause } from './DataGrid.utils';
import { getBrowseRowsQueryKey } from '../../hooks/useRows';

export type DataGridOptions = {
  where?: WhereClause[];
  offset?: number;
  limit?: number;
  order_by?: OrderBy[];
};

export interface DataGridProps {
  table: Table;
  dataSourceName: string;
  onRelationshipOpen?: (props: {
    table: Table;
    dataSourceName: string;
    options?: DataGridOptions;
    relationshipName: string;
    parentOptions?: DataGridOptions;
    parentTableFilter: WhereClause[];
  }) => void;
  onRelationshipClose?: (relationshipName: string) => void;
  options?: DataGridOptions;
  activeRelationships?: string[];
  disableRunQuery?: boolean;
  updateOptions?: (options: DataGridOptions) => void;
  primaryKeys: string[];
}

const getEqualToOperator = (operators: Operator[]) => {
  const equalToOperator = operators.find(op => op.name === 'equals');
  return equalToOperator?.value ?? '_eq';
};

export const DataGrid = (props: DataGridProps) => {
  const {
    dataSourceName,
    table,
    options,
    onRelationshipOpen,
    disableRunQuery,
    activeRelationships,
    onRelationshipClose,
    updateOptions,
    primaryKeys,
  } = props;

  /**
   * States used by the component - passed around and shared by the table and it's options
   */
  const [pageIndex, setPageIndex] = useState(
    disableRunQuery
      ? DEFAULT_PAGE_INDEX
      : (options?.offset ?? DEFAULT_PAGE_INDEX) /
          (options?.limit ?? DEFAULT_PAGE_SIZE)
  );

  const [pageSize, setPageSize] = useState(options?.limit ?? DEFAULT_PAGE_SIZE);
  const [sorting, setSorting] =
    React.useState<ColumnSort[]>(DEFAULT_SORT_CLAUSES);
  const [queryDialogVisibility, setQueryDialogVisibility] = useState(
    DEFAULT_QUERY_DIALOG_STATE
  );
  const [whereClauses, setWhereClauses] = React.useState<WhereClause[]>(
    options?.where ?? DEFAULT_WHERE_CLAUSES
  );
  const [orderByClauses, setOrderClauses] = React.useState<OrderBy[]>(
    options?.order_by ?? DEFAULT_ORDER_BY_CLAUSES
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
    where: whereClauses,
  };

  /**
   * React query hook wrapper to get the rows for
   * the current props.table and props.dataSourceName
   */
  const {
    data: rows,
    isLoading,
    isError,
  } = useRows({
    table,
    dataSourceName,
    options: rowOptions,
  });

  const { data: tableColumnQueryResult } = useTableColumns({
    table,
    dataSourceName,
  });

  const queryClient = useQueryClient();
  const { fireNotification } = useFireNotification();

  const { deleteRow } = useDeleteRow({
    dataSourceName,
    table,
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Successfully deleted row',
        type: 'success',
      });
      queryClient.invalidateQueries(
        getBrowseRowsQueryKey({
          dataSourceName,
          table,
          columns: undefined,
          options,
        })
      );
    },
    onError: (err: Error) => {
      fireNotification({
        title: 'Error!',
        message: err.message,
        type: 'error',
      });
    },
  });

  const { data: relationships, isFetching } = useListAllDatabaseRelationships({
    table,
    dataSourceName,
  });

  useEffect(() => {
    if (!disableRunQuery)
      updateOptions?.({
        limit: pageSize,
        offset: pageIndex * pageSize,
        order_by: orderByClauses,
        where: whereClauses,
      });
  }, [pageIndex, pageSize]);

  const columnNames = (tableColumnQueryResult?.columns || []).map(
    column => column.name
  );
  const { onExportRows } = useExportRows({
    columns: columnNames,
    dataSourceName,
    options: {
      where: whereClauses,
      order_by: orderByClauses,
    },
    table,
  });

  const [selectedRowsLength, setSelectedRowsLength] = useState(0);
  const [selectedRowsWhereClause, setSelectedRowsWhereClause] = useState<
    WhereClause[]
  >([]);
  const { onExportRows: onExportSelectedRows } = useExportRows({
    columns: columnNames,
    dataSourceName,
    options: {
      where: selectedRowsWhereClause,
      order_by: orderByClauses,
    },
    table,
  });

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
      tableColumnQueryResult?.supportedOperators ?? []
    );

    const currentTablePrimaryKeys =
      tableColumnQueryResult?.columns
        .filter(col => col.isPrimaryKey)
        .map(col => col.name) ?? [];

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
      rowData
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

    onRelationshipOpen?.({
      /**
       * dataSourceName is the same as the current table's since it's a local relationship
       */
      dataSourceName,
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

  if (rows === Feature.NotImplemented)
    return <IndicatorCard status="info" headline="Feature Not Implemented" />;

  const onRowsSelect = (rowsId: Record<number, boolean>) => {
    setSelectedRowsLength(Object.keys(rowsId).length);
    const whereClause = adaptSelectedRowIdsToWhereClause({
      rowsId,
      rows,
      primaryKeys,
    });
    setSelectedRowsWhereClause(whereClause);
  };

  return (
    <div>
      <DataTableOptions
        pagination={{
          goToNextPage: () => {
            setPageIndex(currentPage => currentPage + 1);
          },
          goToPreviousPage: () => {
            setPageIndex(currentPage => currentPage - 1);
          },
          isNextPageDisabled: (rows ?? []).length < pageSize,
          isPreviousPageDisabled: pageIndex <= 0,
          pageSize,
          setPageSize,
        }}
        query={{
          disableRunQuery,
          onQuerySearch: () => {
            setQueryDialogVisibility(true);
          },
          onRefreshQueryOptions: refreshAllOptions,
          orderByClauses,
          whereClauses,
          supportedOperators: tableColumnQueryResult?.supportedOperators ?? [],
          removeWhereClause: id => {
            const newWhereClauses = whereClauses.filter((_, i) => i !== id);
            setWhereClauses(whereClauses.filter((_, i) => i !== id));
            updateOptions?.({
              limit: pageSize,
              offset: pageIndex * pageSize,
              where: newWhereClauses,
              order_by: orderByClauses,
            });
          },
          removeOrderByClause: id => {
            const newOrderByClauses = orderByClauses.filter((_, i) => i !== id);
            setOrderClauses(newOrderByClauses);
            updateOptions?.({
              limit: pageSize,
              offset: pageIndex * pageSize,
              where: whereClauses,
              order_by: newOrderByClauses,
            });
          },
          onExportRows,
          onExportSelectedRows,
          disableExportSelectedRows: selectedRowsLength === 0,
        }}
      />

      {isLoading || isFetching ? (
        <div className="my-4">
          <Skeleton height={30} count={8} className="my-2" />
        </div>
      ) : isError ? (
        <div className="my-4">
          <IndicatorCard status="negative" headline="Something went wrong">
            Unable to fetch GraphQL response for table
          </IndicatorCard>
        </div>
      ) : (
        <ReactTableWrapper
          rows={rows ?? []}
          sort={{
            sorting,
            setSorting,
          }}
          relationships={{
            allRelationships: relationships ?? [],
            activeRelationships,
            onClose: relationshipName => {
              onRelationshipClose?.(relationshipName);
            },
            onClick: handleOnRelationshipClick,
          }}
          onRowsSelect={onRowsSelect}
          onRowDelete={deleteRow}
          isRowsSelectionEnabled={primaryKeys.length > 0}
          tableColumns={tableColumnQueryResult?.columns ?? []}
        />
      )}

      {queryDialogVisibility && (
        <QueryDialog
          onClose={() => {
            setQueryDialogVisibility(false);
          }}
          table={table}
          dataSourceName={dataSourceName}
          onSubmit={data => {
            const { filters, sorts } = data;
            setWhereClauses(
              filters.map(filter => {
                return {
                  [filter.column]: {
                    [filter.operator]: filter.value ?? '',
                  },
                };
              })
            );
            setOrderClauses(sorts);

            if (!disableRunQuery)
              updateOptions?.({
                limit: pageSize,
                offset: pageIndex * pageSize,
                order_by: sorts,
                where: filters.map(filter => {
                  return {
                    [filter.column]: {
                      [filter.operator]: filter.value ?? '',
                    },
                  };
                }),
              });

            setQueryDialogVisibility(false);
          }}
          filters={whereClauses?.map(clause => {
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
