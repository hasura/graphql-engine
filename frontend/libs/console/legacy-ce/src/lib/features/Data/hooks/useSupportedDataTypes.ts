import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, Feature, TableColumn } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';
import { getDefaultQueryOptions } from '../reactQueryUtils';

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
    [dataSourceName, 'supported_data_types'],
    async () => {
      const result = await DataSource(httpClient).getSupportedDataTypes({
        dataSourceName,
      });
      return result;
    },
    {
      select,
      ...getDefaultQueryOptions(),
      ...options,
    }
  );
};
