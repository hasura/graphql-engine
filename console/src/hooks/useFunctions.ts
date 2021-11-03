import { getRunSqlQuery } from '@/components/Common/utils/v1QueryUtils';
import { DataSourcesAPI, Driver, useDataSource } from '@/dataSources';
import { PGFunction } from '@/dataSources/services/postgresql/types';
import Endpoints from '@/Endpoints';
import { QualifiedFunction } from '@/metadata/types';
import { useAppSelector } from '@/store';
import { useQuery, UseQueryOptions, UseQueryResult } from 'react-query';
import { Api } from './apiUtils';

interface FetchFunctionsArgs {
  dataSource: DataSourcesAPI;
  //   body: ReturnType<typeof getRunSqlQuery>;
  headers: Record<string, string>;
  trackable: boolean;
  schemaList: string[];
  currentDataSource: string;
  driver: Driver;
}

interface FetchSingleFunctionArg
  extends Omit<FetchFunctionsArgs, 'trackable' | 'schemaList'> {
  name: string;
  schema: string;
}

const fetchFunctions = (args: FetchFunctionsArgs) => () => {
  const {
    dataSource,
    headers,
    schemaList,
    driver,
    currentDataSource,
    trackable,
  } = args;
  if (!dataSource.getFunctionDefinitionSql)
    return Promise.resolve([] as PGFunction[]);
  const fnType = trackable ? 'trackable' : 'non-trackable';
  const body = getRunSqlQuery(
    dataSource.getFunctionDefinitionSql!(schemaList, null, fnType),
    currentDataSource,
    false,
    true,
    driver
  );
  return Api.post<{ result: [[string], [string]] }, PGFunction[]>(
    {
      url: Endpoints.query,
      headers,
      body,
    },
    data => JSON.parse(data.result[1]?.[0]) as PGFunction[]
  );
};

const fetchSingleFunction = (args: FetchSingleFunctionArg) => (): Promise<
  PGFunction | undefined
> => {
  const { dataSource, headers, driver, currentDataSource, schema, name } = args;
  if (!dataSource.getFunctionDefinitionSql) return Promise.resolve(undefined);
  const body = getRunSqlQuery(
    dataSource.getFunctionDefinitionSql!(schema, name),
    currentDataSource,
    false,
    true,
    driver
  );
  return Api.post<{ result: [[string], [string]] }, PGFunction>(
    {
      url: Endpoints.query,
      headers,
      body,
    },
    data => JSON.parse(data.result[1]?.[0])?.[0]
  );
};

export function useTrackableFunctions(
  schemaList: string[],
  queryOptions?: Omit<
    UseQueryOptions<PGFunction[], Error>,
    'queryKey' | 'queryFn'
  >
) {
  const { dataSource, driver } = useDataSource();
  const currentDataSource: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const headers = useAppSelector(state => state.tables.dataHeaders);
  return useQuery(
    ['trackableFunctions', { schemaList }],
    fetchFunctions({
      dataSource,
      driver,
      headers,
      currentDataSource,
      trackable: true,
      schemaList,
    }),
    queryOptions
  );
}

export function useNonTrackableFunctions(
  schemaList: string[],
  queryOptions?: Omit<
    UseQueryOptions<PGFunction[], Error>,
    'queryKey' | 'queryFn'
  >
) {
  const { dataSource, driver } = useDataSource();
  const currentDataSource: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const headers = useAppSelector(state => state.tables.dataHeaders);
  return useQuery(
    ['nonTrackableFunctions', { schemaList }],
    fetchFunctions({
      dataSource,
      driver,
      headers,
      currentDataSource,
      schemaList,
      trackable: false,
    }),
    queryOptions
  );
}

export function useAllFunctions(
  schemaList: string[],
  queryOptions?: Omit<
    UseQueryOptions<PGFunction[], Error>,
    'queryKey' | 'queryFn'
  >
): Pick<
  UseQueryResult<PGFunction[], Error>,
  'data' | 'isLoading' | 'isSuccess' | 'isError' | 'isIdle'
> {
  const { data: trackableFns, ...rest } = useTrackableFunctions(
    schemaList,
    queryOptions
  );
  const { data: nonTrackableFns, ...rem } = useNonTrackableFunctions(
    schemaList,
    queryOptions
  );
  return {
    data: (trackableFns ?? []).concat(nonTrackableFns ?? []),
    isLoading: rest.isLoading || rem.isLoading,
    isSuccess: rest.isSuccess && rem.isSuccess,
    isIdle: rest.isIdle || rem.isIdle,
    isError: rest.isError || rem.isError,
  };
}

export function useFunction(
  args: QualifiedFunction,
  queryOptions?: Omit<
    UseQueryOptions<PGFunction | undefined, Error>,
    'queryKey' | 'queryFn' | 'refetchInterval'
  >
) {
  const { dataSource, driver } = useDataSource();
  const currentDataSource: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const headers = useAppSelector(state => state.tables.dataHeaders);
  return useQuery(
    ['function', args],
    fetchSingleFunction({
      dataSource,
      headers,
      currentDataSource,
      driver,
      ...args,
    }),
    queryOptions
  );
}
