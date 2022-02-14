import { services } from './../dataSources/services/index';
import { RunSQLQueryOptions, useRunSQL } from './common';
import { QualifiedDataSource } from './types';

export function useSchemaList(
  args: QualifiedDataSource,
  queryOptions?: RunSQLQueryOptions<
    Readonly<[key: 'schemaList', dataSource: string, driver: string]>,
    string[]
  >
) {
  const { source, driver } = args;
  const dataSource = services[driver];

  return useRunSQL({
    sql: () => dataSource.schemaListQuery,
    queryKey: ['schemaList', source, driver],
    transformFn: data => data.result?.slice(1).map(d => d[0]) ?? [],
    queryOptions,
    dataSource: args,
  });
}
