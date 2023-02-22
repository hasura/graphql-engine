import type { QualifiedTable } from '../metadata/types';
import type { CheckConstraint } from '../dataSources/types';
import { dataSourceSqlQueries } from '../features/SqlQueries';
import type { QualifiedDataSource, RunSQLResponse } from './types';
import { RunSQLQueryOptions, useRunSQL } from './common';

type CheckQueryOptions<T, N> = RunSQLQueryOptions<
  [name: N, currentDataSource: string, schemasOrTable: T, driver: string],
  CheckConstraint[]
>;

const transformCheckKeys = (data: RunSQLResponse): CheckConstraint[] => {
  return JSON.parse(data.result?.[1]?.[0] ?? '[]');
};
const transformFilterCheckKeys =
  (table: QualifiedTable) =>
  (data: RunSQLResponse): CheckConstraint[] => {
    return transformCheckKeys(data).filter(
      key => key.table_name === table.name && key.table_schema === table.schema
    );
  };

function useCheckConstraintKeysBase<T extends string[] | QualifiedTable, N>(
  schemasOrTable: T,
  name: N,
  transformFn: (data: RunSQLResponse) => CheckConstraint[],
  dataSource: QualifiedDataSource,
  queryOptions?: CheckQueryOptions<T, N>
) {
  const { source, driver } = dataSource;
  const targetDataSource = dataSourceSqlQueries[driver];
  const sql = () =>
    Array.isArray(schemasOrTable)
      ? targetDataSource.checkConstraintsSql({ schemas: schemasOrTable })
      : targetDataSource.checkConstraintsSql({ tables: [schemasOrTable] });
  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable, driver],
    transformFn,
    queryOptions,
    dataSource,
  });
}

export function useDataSourceCheckConstraints(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: CheckQueryOptions<string[], 'dataSourceUniqueKeys'>
) {
  const { schemas, ...dataSource } = args;
  return useCheckConstraintKeysBase(
    schemas,
    'dataSourceUniqueKeys',
    transformCheckKeys,
    dataSource,
    queryOptions
  );
}

export function useTableCheckConstraints(
  args: { table: QualifiedTable } & QualifiedDataSource,
  queryOptions?: CheckQueryOptions<QualifiedTable, 'tableUniqueKeys'>
) {
  const { table, ...dataSource } = args;
  return useCheckConstraintKeysBase(
    table,
    'tableUniqueKeys',
    transformFilterCheckKeys(table),
    dataSource,
    queryOptions
  );
}
