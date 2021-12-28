import { getRunSqlQuery } from '@/components/Common/utils/v1QueryUtils';
import { useDataSource } from '@/dataSources';
import Endpoints from '@/Endpoints';
import { useQuery, UseQueryOptions } from 'react-query';
import { useAppSelector } from '../store';
import { Api } from './apiUtils';

export function useSchemaList(
  queryOptions?: UseQueryOptions<
    string[],
    Error,
    string[],
    ['schemaList', string, string]
  >
) {
  const url = Endpoints.query;
  const { dataSource, driver } = useDataSource();

  const currentDataSource: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const headers = useAppSelector(state => state.tables.dataHeaders);

  const body = getRunSqlQuery(
    dataSource.schemaListQuery,
    currentDataSource,
    false,
    true,
    driver
  );

  const fetchSchemaList = () => {
    return Api.post<{ result: string[][] }, string[]>(
      { headers, body, url },
      data => data.result.slice(1).map(d => d[0])
    );
  };

  return useQuery(
    ['schemaList', currentDataSource, driver],
    fetchSchemaList,
    queryOptions
  );
}
