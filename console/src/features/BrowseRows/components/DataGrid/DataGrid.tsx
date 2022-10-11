import { useRows } from '@/components/Services/Data/TableBrowseRows/Hooks';
import { Feature, OrderBy, WhereClause } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { PaginationState, SortingState } from '@tanstack/react-table';
import React, { useEffect, useState } from 'react';
import { DataTable } from './parts/DataTable';

import { QueryDialog } from './QueryDialog';

interface DataGridProps {
  table: Table;
  dataSourceName: string;
}

export const DataGrid = (props: DataGridProps) => {
  const [{ pageIndex, pageSize }, setPagination] = useState<PaginationState>({
    pageIndex: 0,
    pageSize: 10,
  });
  const [sorting, setSorting] = React.useState<SortingState>([]);
  const [queryDialogVisibility, setQueryDialogVisibility] = useState(false);
  const [whereClauses, setWhereClauses] = React.useState<WhereClause[]>();
  const [orderByClauses, setOrderClauses] = React.useState<OrderBy[]>([]);
  const pagination = React.useMemo(
    () => ({
      pageIndex,
      pageSize,
    }),
    [pageIndex, pageSize]
  );

  const { dataSourceName, table } = props;

  useEffect(() => {
    setPagination({
      pageIndex: 0,
      pageSize: 10,
    });
    setSorting([]);
    setQueryDialogVisibility(false);
    setWhereClauses([]);
    setOrderClauses([]);
  }, [dataSourceName, table]);

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
      order_by: [
        ...orderByClauses,
        ...sorting.map<OrderBy>(sort => ({
          column: sort.id,
          type: sort.desc ? 'desc' : 'asc',
        })),
      ],

      where: whereClauses,
    },
  });

  if (rows === Feature.NotImplemented)
    return <IndicatorCard status="info" headline="Feature Not Implemented" />;

  if (isError)
    return <IndicatorCard status="negative" headline="Something went wrong" />;

  const columns = rows?.length ? Object.keys(rows[0]) : [];

  return (
    <div>
      <DataTable
        rows={rows ?? []}
        columns={columns}
        pagination={pagination}
        setPagination={setPagination}
        sorting={sorting}
        setSorting={setSorting}
        onQueryClick={() => {
          setQueryDialogVisibility(true);
        }}
        resetQuery={() => {
          setWhereClauses(undefined);
          setOrderClauses([]);
        }}
        isLoading={isLoading}
      />
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
