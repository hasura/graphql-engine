import type { QualifiedTable } from '../metadata/types';
import { dataSourceSqlQueries } from '../features/SqlQueries';
import { Driver } from '../dataSources';
import { MSSqlConstraint } from '../components/Services/Data/mergeData';
import type { UniqueKey } from './../dataSources/types';
import type { QualifiedDataSource, RunSQLResponse } from './types';
import {
  RunSQLQueryOptions,
  transformMssqlConstraint,
  useRunSQL,
} from './common';

type UniqueQueryOptions<T, N> = RunSQLQueryOptions<
  [name: N, currentDataSource: string, schemasOrTable: T, driver: string],
  UniqueKey[]
>;

const transformUniqueKeys =
  (driver: Driver) =>
  (data: RunSQLResponse): UniqueKey[] => {
    const parsedPKs: MSSqlConstraint[] | UniqueKey[] = JSON.parse(
      data.result?.[1]?.[0] ?? '[]'
    );
    if (driver === 'mssql') return transformMssqlConstraint(data);
    return parsedPKs as UniqueKey[];
  };
const transformFilterUniqueKeys =
  (table: QualifiedTable, driver: Driver) =>
  (data: RunSQLResponse): UniqueKey[] => {
    return transformUniqueKeys(driver)(data).filter(
      key => key.table_name === table.name && key.table_schema === table.schema
    );
  };

export function useDataSourceUniqueKeys(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: UniqueQueryOptions<string[], 'dataSourceUniqueKeys'>
) {
  const { schemas, driver, source } = args;
  const targetDataSource = dataSourceSqlQueries[driver];
  const sql = () => targetDataSource.uniqueKeysSql({ schemas });
  return useRunSQL({
    sql,
    queryKey: ['dataSourceUniqueKeys', source, schemas, driver],
    transformFn: transformUniqueKeys(driver),
    queryOptions,
    dataSource: { driver, source },
  });
}

export function useTableUniqueKeys(
  args: { table: QualifiedTable } & QualifiedDataSource,
  queryOptions?: UniqueQueryOptions<QualifiedTable, 'tableUniqueKeys'>
) {
  const { table, driver, source } = args;
  const dataSource = dataSourceSqlQueries[driver];
  const sql = () => dataSource.uniqueKeysSql({ tables: [table] });
  return useRunSQL({
    sql,
    queryKey: ['tableUniqueKeys', source, table, driver],
    transformFn: transformFilterUniqueKeys(table, driver),
    queryOptions,
    dataSource: { driver, source },
  });
}
