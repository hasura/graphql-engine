import { getRunSqlQuery } from '../components/Common/utils/v1QueryUtils';
import Endpoints from '../Endpoints';
import { useAppSelector } from '../storeHooks';
import { Constraint } from '../dataSources/types';
import { MSSqlConstraint } from '../components/Services/Data/mergeData';
import type { UseQueryOptions } from 'react-query';
import { useQuery } from 'react-query';
import { APIError } from './error';
import { Api } from './apiUtils';
import { QualifiedDataSource, RunSQLResponse } from './types';

export type RunSQLQueryOptions<KEY extends readonly unknown[], DATA> = Omit<
  UseQueryOptions<DATA, APIError, DATA, KEY>,
  'queryKey' | 'queryFn'
>;

type UseRunSQLArg<K extends readonly unknown[], D> = {
  sql: () => string;
  queryKey: K;
  transformFn: (data: RunSQLResponse) => D;
  queryOptions?: RunSQLQueryOptions<K, D>;
  dataSource: QualifiedDataSource;
  fallBack?: { shouldFallback: boolean; default: D };
};

export function useRunSQL<K extends readonly unknown[], D>({
  sql,
  queryKey,
  transformFn,
  queryOptions,
  dataSource,
  fallBack,
}: UseRunSQLArg<K, D>) {
  const headers = useAppSelector(state => state.tables.dataHeaders);
  const { source, driver } = dataSource;

  return useQuery({
    ...queryOptions,
    queryKey,
    queryFn() {
      if (fallBack?.shouldFallback) return Promise.resolve(fallBack.default);
      const body = getRunSqlQuery(sql(), source, false, true, driver);
      return Api.post<RunSQLResponse, D>(
        {
          url: Endpoints.query,
          headers,
          body,
        },
        transformFn
      );
    },
  });
}

export const transformMssqlConstraint = (data: RunSQLResponse) => {
  const parsedPKs: MSSqlConstraint[] = JSON.parse(
    data.result?.[1]?.[0] ?? '[]'
  );
  return (parsedPKs as MSSqlConstraint[]).reduce((acc: Constraint[], pk) => {
    const { table_name, table_schema, constraints } = pk;

    const columnsByConstraintName: { [name: string]: string[] } = {};
    constraints.forEach(c => {
      columnsByConstraintName[c.constraint_name] = [
        ...(columnsByConstraintName[c.constraint_name] || []),
        c.name,
      ];
    });

    const constraintInfo = Object.keys(columnsByConstraintName).map(pkName => ({
      table_schema,
      table_name,
      constraint_name: pkName,
      columns: columnsByConstraintName[pkName],
    }));
    return [...acc, ...constraintInfo];
  }, []);
};
