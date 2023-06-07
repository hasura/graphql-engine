import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, Feature, IntrospectedTable } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';

export const useIntrospectedTables = <
  FinalResult = Feature | IntrospectedTable[]
>({
  dataSourceName,
  select,
  options = {},
}: {
  dataSourceName: string;
  select?: (data: Feature | IntrospectedTable[]) => FinalResult;
  options?: UseQueryOptions<
    Feature | IntrospectedTable[],
    APIError,
    FinalResult
  >;
}) => {
  const httpClient = useHttpClient();

  return useQuery<Feature | IntrospectedTable[], APIError, FinalResult>(
    [dataSourceName, 'untracked_tables'],
    async () => {
      const result = await DataSource(httpClient).introspectTables({
        dataSourceName,
      });
      return result;
    },
    {
      select,
      ...options,
    }
  );
};
