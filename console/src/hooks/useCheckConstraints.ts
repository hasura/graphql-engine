import { currentDriver } from '@/dataSources';
import type { QualifiedTable } from '@/metadata/types';
import { useAppSelector } from '@/store';
import type { CheckConstraint } from '@/dataSources/types';
import { dataSourceSqlQueries } from '@/features/SqlQueries';
import type { RunSQLResponse } from './types';
import { RunSQLQueryOptions, useRunSQL } from './common';

type CheckQueryOptions<T, N> = RunSQLQueryOptions<
  [name: N, currentDataSource: string, schemasOrTable: T],
  CheckConstraint[]
>;

const transformCheckKeys = (data: RunSQLResponse): CheckConstraint[] => {
  return JSON.parse(data.result?.[1]?.[0] ?? '[]');
};
const transformFilterCheckKeys = (table: QualifiedTable) => (
  data: RunSQLResponse
): CheckConstraint[] => {
  return transformCheckKeys(data).filter(
    key => key.table_name === table.name && key.table_schema === table.schema
  );
};

function useCheckConstraintKeysBase<T extends string[] | QualifiedTable, N>(
  schemasOrTable: T,
  name: N,
  transformFn: (data: RunSQLResponse) => CheckConstraint[],
  queryOptions?: CheckQueryOptions<T, N>
) {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const dataSource = dataSourceSqlQueries[currentDriver];
  const sql = () =>
    Array.isArray(schemasOrTable)
      ? dataSource.checkConstraintsSql({ schemas: schemasOrTable })
      : dataSource.checkConstraintsSql({ tables: [schemasOrTable] });
  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable],
    transformFn,
    queryOptions,
  });
}

export function useDataSourceCheckConstraints(
  schemas: string[],
  queryOptions?: CheckQueryOptions<string[], 'dataSourceUniqueKeys'>
) {
  return useCheckConstraintKeysBase(
    schemas,
    'dataSourceUniqueKeys',
    transformCheckKeys,
    queryOptions
  );
}

export function useTableCheckConstraints(
  table: QualifiedTable,
  queryOptions?: CheckQueryOptions<QualifiedTable, 'tableUniqueKeys'>
) {
  return useCheckConstraintKeysBase(
    table,
    'tableUniqueKeys',
    transformFilterCheckKeys(table),
    queryOptions
  );
}
