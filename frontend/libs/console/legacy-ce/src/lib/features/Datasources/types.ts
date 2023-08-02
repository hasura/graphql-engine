import { APIError } from '../../hooks/error';
import { UseQueryOptions } from 'react-query';

export type RunSQLQueryOptions<KEY extends readonly unknown[], DATA> = Omit<
  UseQueryOptions<DATA, APIError, DATA, KEY>,
  'queryKey' | 'queryFn'
>;

export type RunSQLResponse =
  | {
      result: string[][];
      result_type: 'TuplesOk';
    }
  | {
      result_type: 'CommandOk';
      result: null;
    };

export type QualifiedDataSource = {
  name: string;
  driver:
    | 'postgres'
    | 'mssql'
    | 'bigquery'
    | 'citus'
    | 'cockroach'
    | 'alloy'
    | 'mysql';
};

export type UseRunSQLArg<K extends readonly unknown[], D> = {
  sql: string;
  queryKey: K;
  transformFn: (data: RunSQLResponse) => D;
  queryOptions?: RunSQLQueryOptions<K, D>;
  dataSource: QualifiedDataSource;
  fallBack?: { shouldFallback: boolean; default: D };
};
