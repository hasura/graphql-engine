import { UseQueryOptions, useQuery } from 'react-query';
import { APIError } from '../../../hooks/error';
import { DataSource, Feature } from '../../DataSource';
import { Capabilities } from '@hasura/dc-api-types';
import { useHttpClient } from '../../Network';
import { useMetadata } from '../../hasura-metadata-api';

type AllCapabilitiesReturnType = {
  driver: string;
  capabilities: Feature | Capabilities;
}[];

export const useAllDriverCapabilities = <
  FinalResult = AllCapabilitiesReturnType
>({
  select,
  options = {},
}: {
  select?: (data: AllCapabilitiesReturnType) => FinalResult;
  options?: UseQueryOptions<AllCapabilitiesReturnType, APIError, FinalResult>;
}) => {
  const httpClient = useHttpClient();

  const { data: driverNames = [], isFetching: isFetchingMetadata } =
    useMetadata(m => m.metadata.sources.map(source => source.kind));

  return useQuery<AllCapabilitiesReturnType, APIError, FinalResult>(
    [driverNames, 'all_capabilities'],
    async () => {
      const result = driverNames.map(async driverName => {
        const capabilities =
          (await DataSource(httpClient).getDriverCapabilities(
            driverName ?? ''
          )) ?? Feature.NotImplemented;

        return { driver: driverName, capabilities };
      });

      const finalResult = await Promise.all(result);

      return finalResult;
    },
    {
      enabled: !isFetchingMetadata && options.enabled,
      select,
      ...options,
    }
  );
};
