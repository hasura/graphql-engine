import { getRunSqlQuery } from '@/components/Common/utils/v1QueryUtils';
import Endpoints from '@/Endpoints';
import { useAppSelector } from '@/store';
import type { UseQueryOptions } from 'react-query';
import { useQuery } from 'react-query';
import { Api } from './apiUtils';
import { RunSQLResponse } from './types';

export type RunSQLQueryOptions<KEY extends readonly unknown[], DATA> = Omit<
  UseQueryOptions<DATA, Error, DATA, KEY>,
  'queryKey' | 'queryFn'
>;

type UseRunSQLArg<K extends readonly unknown[], D> = {
  sql: () => string;
  queryKey: K;
  transformFn: (data: RunSQLResponse) => D;
  queryOptions?: RunSQLQueryOptions<K, D>;
};

export function useRunSQL<K extends readonly unknown[], D>({
  sql,
  queryKey,
  transformFn,
  queryOptions,
}: UseRunSQLArg<K, D>) {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const headers = useAppSelector(state => state.tables.dataHeaders);

  return useQuery({
    ...queryOptions,
    queryKey,
    queryFn() {
      const body = getRunSqlQuery(sql(), source, false, true);
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
