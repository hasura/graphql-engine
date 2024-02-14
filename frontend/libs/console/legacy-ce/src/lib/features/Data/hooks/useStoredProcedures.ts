import { UseQueryOptions, useQuery } from 'react-query';
import { getDefaultQueryOptions } from '../reactQueryUtils';
import { DataSource, Feature } from '../../DataSource';
import { APIError } from '../../../hooks/error';
import { useHttpClient } from '../../Network';
type StoredProcedure = unknown;

export const useStoredProcedures = <FinalResult = Feature | StoredProcedure[]>({
  dataSourceName,
  select,
  options = {},
}: {
  dataSourceName: string;
  select?: (data: Feature | StoredProcedure[]) => FinalResult;
  options?: UseQueryOptions<Feature | StoredProcedure[], APIError, FinalResult>;
}) => {
  const httpClient = useHttpClient();

  return useQuery<Feature | StoredProcedure[], APIError, FinalResult>(
    [dataSourceName, 'stored_procedures'],
    async () => {
      const result = await DataSource(httpClient).getStoredProcedures({
        dataSourceName,
      });
      return result;
    },
    {
      ...getDefaultQueryOptions(),
      ...options,
      select,
    }
  );
};
