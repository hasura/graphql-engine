import { currentDriver, useDataSource } from '@/dataSources';
import { RunSQLQueryOptions, useRunSQL } from './common';
import { useAppSelector } from '../store';

export function useSchemaList(
  queryOptions?: RunSQLQueryOptions<
    Readonly<[key: 'schemaList', dataSource: string, driver: string]>,
    string[]
  >
) {
  const { dataSource } = useDataSource();

  const currentDataSource: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useRunSQL({
    sql: () => dataSource.schemaListQuery,
    queryKey: ['schemaList', currentDataSource, currentDriver],
    transformFn: data => data.result?.slice(1).map(d => d[0]) ?? [],
    queryOptions,
  });
}
