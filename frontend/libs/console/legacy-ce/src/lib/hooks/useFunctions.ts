import { services } from '../dataSources/services';
import { PGFunction } from '../dataSources/services/postgresql/types';
import { QualifiedFunction } from '../metadata/types';
import { UseQueryResult } from 'react-query';
import { APIError } from './error';
import { QualifiedDataSource, RunSQLResponse } from './types';
import { RunSQLQueryOptions, useRunSQL } from './common';

type FnQueryOptions<T, D> = RunSQLQueryOptions<
  [name: string, source: string, schemasOrFunction: T, driver: string],
  D
>;

const transFormFns = (data: RunSQLResponse) => {
  return JSON.parse(data.result?.[1]?.[0] ?? '[]') as PGFunction[];
};

const transFormSingleFn = (data: RunSQLResponse) => {
  return transFormFns(data)?.[0] ?? null;
};

function useFunctionBase<T extends string[] | QualifiedFunction, D>(
  fnOrSchemas: T,
  name: string,
  transformFn: (r: RunSQLResponse) => D,
  dataSource: QualifiedDataSource,
  fallBackData: D,
  queryOptions?: FnQueryOptions<T, D>,
  fnType?: 'trackable' | 'non-trackable'
) {
  const { source, driver } = dataSource;
  const targetDataSource = services[driver];

  const fnSqlString = Array.isArray(fnOrSchemas)
    ? targetDataSource.getFunctionDefinitionSql?.(fnOrSchemas, null, fnType)
    : targetDataSource.getFunctionDefinitionSql?.(
        fnOrSchemas?.schema,
        fnOrSchemas?.name
      );

  const sql = () => {
    return fnSqlString ?? '';
  };

  return useRunSQL({
    sql,
    queryKey: [name, source, fnOrSchemas, driver],
    transformFn,
    queryOptions,
    dataSource,
    fallBack: { shouldFallback: !fnSqlString, default: fallBackData },
  });
}

export function useTrackableFunctions(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: FnQueryOptions<string[], PGFunction[]>
) {
  const { schemas, ...dataSource } = args;
  return useFunctionBase(
    schemas,
    'trackableFunctions',
    transFormFns,
    dataSource,
    [],
    queryOptions,
    'trackable'
  );
}

export function useNonTrackableFunctions(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: FnQueryOptions<string[], PGFunction[]>
) {
  const { schemas, ...dataSource } = args;
  return useFunctionBase(
    schemas,
    'nonTrackableFunctions',
    transFormFns,
    dataSource,
    [],
    queryOptions,
    'non-trackable'
  );
}

//! Prefer using useTrackableFunctions and useNonTrackableFunctions in combination to using this
export function useAllFunctions(
  args: { schemas: string[] } & QualifiedDataSource,
  queryOptions?: FnQueryOptions<string[], PGFunction[]>
): Pick<
  UseQueryResult<PGFunction[], APIError>,
  'data' | 'isLoading' | 'isSuccess' | 'isError' | 'isIdle' | 'error'
> {
  const { data: trackableFns, ...rest } = useTrackableFunctions(
    args,
    queryOptions
  );
  const { data: nonTrackableFns, ...rem } = useNonTrackableFunctions(
    args,
    queryOptions
  );
  return {
    data: (trackableFns ?? []).concat(nonTrackableFns ?? []),
    isLoading: rest.isLoading || rem.isLoading,
    isSuccess: rest.isSuccess && rem.isSuccess,
    isIdle: rest.isIdle || rem.isIdle,
    isError: rest.isError || rem.isError,
    error: rest.error || rem.error,
  };
}

export function useSingleFunction(
  args: { fn: QualifiedFunction } & QualifiedDataSource,
  queryOptions?: FnQueryOptions<QualifiedFunction, PGFunction | null>
) {
  const { fn, ...dataSource } = args;
  return useFunctionBase(
    fn,
    'function',
    transFormSingleFn,
    dataSource,
    null,
    queryOptions
  );
}
