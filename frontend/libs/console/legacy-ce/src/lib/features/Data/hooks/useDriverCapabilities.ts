import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, Feature } from '../../DataSource';
import { useMetadata } from '../../hasura-metadata-api';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';
import { Capabilities } from '@hasura/dc-api-types';

type UseDatabaseCapabilitiesArgs = {
  dataSourceName: string;
};

export const useDriverCapabilities = <FinalResult = Feature | Capabilities>({
  dataSourceName,
  select,
  options = {},
}: UseDatabaseCapabilitiesArgs & {
  select?: (data: Feature | Capabilities) => FinalResult;
  options?: UseQueryOptions<Feature | Capabilities, APIError, FinalResult>;
}) => {
  const httpClient = useHttpClient();

  const { data: driverName, isFetching: isFetchingMetadata } = useMetadata(
    m => m.metadata.sources.find(source => source.name === dataSourceName)?.kind
  );

  return useQuery<Feature | Capabilities, APIError, FinalResult>(
    [dataSourceName, 'capabilities'],
    async () => {
      const result =
        (await DataSource(httpClient).getDriverCapabilities(
          driverName ?? ''
        )) ?? Feature.NotImplemented;
      return result;
    },
    {
      enabled: !isFetchingMetadata && options.enabled,
      select,
      ...options,
    }
  );
};
