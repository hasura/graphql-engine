import { UseQueryOptions, useQuery, useQueryClient } from 'react-query';
import { DataSource, Feature, IntrospectedFunction } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';
import { DEFAULT_STALE_TIME } from '../../hasura-metadata-api/useInconsistentMetadata';

export const useInvalidateIntrospectedFunction = () => {
  const queryClient = useQueryClient();
  const invalidateIntrospectedFunction = (dataSourceName: string) => {
    queryClient.invalidateQueries([dataSourceName, 'introspected_functions']);
  };

  return invalidateIntrospectedFunction;
};

export const useIntrospectedFunctions = <
  FinalResult = Feature | IntrospectedFunction[],
>({
  dataSourceName,
  select,
  options = {},
}: {
  dataSourceName: string;
  select?: (data: Feature | IntrospectedFunction[]) => FinalResult;
  options?: UseQueryOptions<
    Feature | IntrospectedFunction[],
    APIError,
    FinalResult
  >;
}) => {
  const httpClient = useHttpClient();

  return useQuery<Feature | IntrospectedFunction[], APIError, FinalResult>(
    [dataSourceName, 'introspected_functions'],
    async () => {
      const result = await DataSource(httpClient).getTrackableFunctions(
        dataSourceName
      );

      return result;
    },
    {
      select,
      staleTime: options.staleTime ?? DEFAULT_STALE_TIME,
      ...options,
    }
  );
};
