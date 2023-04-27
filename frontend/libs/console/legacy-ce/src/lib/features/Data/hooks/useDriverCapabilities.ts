import { UseQueryOptions, useQuery } from 'react-query';
import { DataSource, DriverCapability, Feature } from '../../DataSource';
import { useMetadata } from '../../hasura-metadata-api';
import { useHttpClient } from '../../Network';
import { APIError } from '../../../hooks/error';

type UseDatabaseCapabilitiesArgs = {
  dataSourceName: string;
};

export const useDriverCapabilities = <
  FinalResult = Feature | DriverCapability
>({
  dataSourceName,
  select,
  options = {},
}: UseDatabaseCapabilitiesArgs & {
  select?: (data: Feature | DriverCapability) => FinalResult;
  options?: UseQueryOptions<Feature | DriverCapability, APIError, FinalResult>;
}) => {
  const httpClient = useHttpClient();

  const { data: driverName, isFetching: isFetchingMetadata } = useMetadata(
    m => m.metadata.sources.find(source => source.name === dataSourceName)?.kind
  );

  return useQuery<Feature | DriverCapability, APIError, FinalResult>(
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
