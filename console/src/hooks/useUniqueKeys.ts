import { currentDriver } from '@/dataSources';
import type { QualifiedTable } from '@/metadata/types';
import { useAppSelector } from '@/store';
import { dataSourceSqlQueries } from '@/features/SqlQueries';
import type { UniqueKey } from './../dataSources/types';
import type { RunSQLResponse } from './types';
import { RunSQLQueryOptions, useRunSQL } from './common';

type UniqueQueryOptions<T, N> = RunSQLQueryOptions<
  [name: N, currentDataSource: string, schemasOrTable: T],
  UniqueKey[]
>;

const transformUniqueKeys = (data: RunSQLResponse): UniqueKey[] => {
  return JSON.parse(data.result?.[1]?.[0] ?? '[]');
};
const transformFilterUniqueKeys = (table: QualifiedTable) => (
  data: RunSQLResponse
): UniqueKey[] => {
  return transformUniqueKeys(data).filter(
    key => key.table_name === table.name && key.table_schema === table.schema
  );
};

export function useDataSourceUniqueKeys(
  schemas: string[],
  queryOptions?: UniqueQueryOptions<string[], 'dataSourceUniqueKeys'>
) {
  const dataSource = dataSourceSqlQueries[currentDriver];
  const sql = () => dataSource.uniqueKeysSql({ schemas });
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useRunSQL({
    sql,
    queryKey: ['dataSourceUniqueKeys', source, schemas],
    transformFn: transformUniqueKeys,
    queryOptions,
  });
}

export function useTableUniqueKeys(
  table: QualifiedTable,
  queryOptions?: UniqueQueryOptions<QualifiedTable, 'tableUniqueKeys'>
) {
  const dataSource = dataSourceSqlQueries[currentDriver];
  const sql = () => dataSource.uniqueKeysSql({ tables: [table] });
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useRunSQL({
    sql,
    queryKey: ['tableUniqueKeys', source, table],
    transformFn: transformFilterUniqueKeys(table),
    queryOptions,
  });
}
