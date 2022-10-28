import { useRows } from '@/components/Services/Data/TableBrowseRows/Hooks';
import { Feature, OrderBy, WhereClause } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { ColumnSort } from '@tanstack/react-table';
import React, { useEffect, useState } from 'react';
import Skeleton from 'react-loading-skeleton';
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
import { useTableColumns } from './useTableColumns';
import { transformToOrderByClause } from './utils';

interface DataGridProps {
  table: Table;
  dataSourceName: string;
}

export const DataGrid = (props: DataGridProps) => {
  /**
   * States used by the component - passed around and shared by the table and it's options
   */
  const [pageIndex, setPageIndex] = useState(DEFAULT_PAGE_INDEX);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [sorting, setSorting] =
    React.useState<ColumnSort[]>(DEFAULT_SORT_CLAUSES);
  const [queryDialogVisibility, setQueryDialogVisibility] = useState(
    DEFAULT_QUERY_DIALOG_STATE
  );
  const [whereClauses, setWhereClauses] = React.useState<WhereClause[]>(
    DEFAULT_WHERE_CLAUSES
  );
  const [orderByClauses, setOrderClauses] = React.useState<OrderBy[]>(
    DEFAULT_ORDER_BY_CLAUSES
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
  };

  const { dataSourceName, table } = props;

  /**
   * React preserves the component's state between table to table switch.
   * This helps the component to shed the options in between the user's jump between one table to another
   */
  useEffect(() => {
    refreshAllOptions();
  }, [dataSourceName, table]);

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
    options: {
      limit: pageSize,
      offset: pageIndex * pageSize,
      order_by: [...orderByClauses, ...transformToOrderByClause(sorting)],
      where: whereClauses,
    },
  });

  const { data: tableColumnQuery } = useTableColumns({ table, dataSourceName });

  if (rows === Feature.NotImplemented)
    return <IndicatorCard status="info" headline="Feature Not Implemented" />;

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
          onQuerySearch: () => {
            setQueryDialogVisibility(true);
          },
          onRefreshQueryOptions: refreshAllOptions,
          orderByClauses,
          whereClauses,
          supportedOperators: tableColumnQuery?.supportedOperators ?? [],
          removeWhereClause: id => {
            setWhereClauses(whereClauses.filter((_, i) => i !== id));
          },
          removeOrderByClause: id => {
            setOrderClauses(orderByClauses.filter((_, i) => i !== id));
          },
        }}
      />

      {isLoading ? (
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
