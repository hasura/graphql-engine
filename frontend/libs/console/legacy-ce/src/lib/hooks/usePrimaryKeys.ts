import type { MSSqlConstraint } from '../components/Services/Data/mergeData';
import { Driver } from '../dataSources';
import type { PrimaryKey } from '../dataSources/types';
import { dataSourceSqlQueries } from '../features/SqlQueries';
import type { QualifiedTable } from '../metadata/types';
import {
  RunSQLQueryOptions,
  transformMssqlConstraint,
  useRunSQL,
} from './common';
import type { QualifiedDataSource, RunSQLResponse } from './types';

type PKQueryOptions<T, N, D> = RunSQLQueryOptions<
  [name: N, currentDataSource: string, schemasOrTable: T, driver: string],
  D
>;

const transformPK =
  (driver: Driver) =>
  (data: RunSQLResponse): PrimaryKey[] => {
    const parsedPKs: MSSqlConstraint[] | PrimaryKey[] = JSON.parse(
      data.result?.[1]?.[0] ?? '[]'
    );
    if (driver === 'mssql') return transformMssqlConstraint(data);
    return parsedPKs as PrimaryKey[];
  };

const transformSinglePK =
  (table: QualifiedTable) =>
  (driver: Driver) =>
  (data: RunSQLResponse): PrimaryKey | null => {
    const res =
      transformPK(driver)(data).find(
        key =>
          key.table_name === table.name && key.table_schema === table.schema
      ) ?? null;
    return res;
  };

function usePrimaryKeysBase<T extends string[] | QualifiedTable, N, D>(
  schemasOrTable: T,
  name: N,
  transformFn: (d: Driver) => (r: RunSQLResponse) => D,
  dataSource: QualifiedDataSource,
  queryOptions?: PKQueryOptions<T, N, D>
) {
  const { source, driver } = dataSource;
  const targetDataSource = dataSourceSqlQueries[driver];
  const sql = () =>
    Array.isArray(schemasOrTable)
      ? targetDataSource.primaryKeysInfoSql({ schemas: schemasOrTable })
      : targetDataSource.primaryKeysInfoSql({ tables: [schemasOrTable] });
  return useRunSQL({
    sql,
    queryKey: [name, source, schemasOrTable, driver],
    transformFn: transformFn(driver),
    queryOptions,
    dataSource,
  });
}

export function useDataSourcePrimaryKeys(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: PKQueryOptions<string[], 'dataSourcePrimaryKeys', PrimaryKey[]>
) {
  const { schemas, ...dataSource } = args;
  return usePrimaryKeysBase(
    schemas,
    'dataSourcePrimaryKeys',
    transformPK,
    dataSource,
    queryOptions
  );
}

export function useTablePrimaryKey(
  args: { table: QualifiedTable } & QualifiedDataSource,
  queryOptions?: PKQueryOptions<
    QualifiedTable,
    'tablePrimaryKey',
    PrimaryKey | null
  >
) {
  const { table, ...dataSource } = args;
  return usePrimaryKeysBase(
    table,
    'tablePrimaryKey',
    transformSinglePK(table),
    dataSource,
    queryOptions
  );
}
