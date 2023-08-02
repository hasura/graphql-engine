import { getRunSqlQuery } from '../../components/Common/utils/v1QueryUtils';
import Endpoints from '../../Endpoints';
import { Api } from '../../hooks/apiUtils';
import { useQuery } from 'react-query';
import { useSelector } from 'react-redux';
import { RunSQLResponse, UseRunSQLArg } from './types';

export function useRunSQL<K extends readonly unknown[], D>({
  sql,
  queryKey,
  transformFn,
  queryOptions,
  dataSource,
  fallBack,
}: UseRunSQLArg<K, D>) {
  // Needed to avoid circular dependency
  const headers = useSelector<any>(state => state.tables.dataHeaders) as Record<
    string,
    string
  >;
  const { name, driver } = dataSource;

  return useQuery({
    ...queryOptions,
    queryKey,
    queryFn() {
      if (fallBack?.shouldFallback) return Promise.resolve(fallBack.default);
      const body = getRunSqlQuery(sql, name, false, true, driver);
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
