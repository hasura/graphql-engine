import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, Feature, TableColumn } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';

export const useSupportedDataTypes = <
  FinalResult = Feature | Record<TableColumn['consoleDataType'], string[]>
>({
  dataSourceName,
  select,
  options = {},
}: {
  dataSourceName: string;
  select?: (
    data: Feature | Record<TableColumn['consoleDataType'], string[]>
  ) => FinalResult;
  options?: UseQueryOptions<
    Feature | Record<TableColumn['consoleDataType'], string[]>,
    APIError,
    FinalResult
  >;
}) => {
  const httpClient = useHttpClient();

  return useQuery<
    Feature | Record<TableColumn['consoleDataType'], string[]>,
    APIError,
    FinalResult
  >(
    [dataSourceName, 'untracked_tables'],
    async () => {
      const result = await DataSource(httpClient).getSupportedDataTypes({
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
