import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, Feature } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';
import { getDefaultQueryOptions } from '../reactQueryUtils';

export const useSupportedDataTypes = <FinalResult = string[]>({
  dataSourceName,
  options = {},
}: {
  dataSourceName: string;
  options?: UseQueryOptions<Feature | string[], APIError, FinalResult>;
}) => {
  const httpClient = useHttpClient();

  return useQuery<Feature | string[], APIError, FinalResult>(
    [dataSourceName, 'supported_data_types'],
    async () => {
      const result = await DataSource(httpClient).getSupportedScalars({
        dataSourceName,
      });
      if (result === Feature.NotImplemented) {
        return [];
      }
      // using Set to remove duplicates that result from the way the data types are flattened from definition object
      return Array.from(new Set(result));
    },
    {
      ...getDefaultQueryOptions(),
      ...options,
    }
  );
};
