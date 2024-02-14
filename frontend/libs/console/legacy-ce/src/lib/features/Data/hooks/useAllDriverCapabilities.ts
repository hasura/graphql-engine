import { UseQueryOptions, useQuery } from 'react-query';
import { APIError } from '../../../hooks/error';
import { DataSource, Feature } from '../../DataSource';
import { Capabilities } from '@hasura/dc-api-types';
import { useHttpClient } from '../../Network';
import { useMetadata } from '../../hasura-metadata-api';
import { Source } from '../../hasura-metadata-types';

type AllCapabilitiesReturnType = {
  driver: Source;
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

  const { data: drivers = [], isFetching: isFetchingMetadata } = useMetadata(
    m => m.metadata.sources
  );

  return useQuery<AllCapabilitiesReturnType, APIError, FinalResult>(
    ['all_capabilities'],
    async () => {
      const result = drivers.map(async driver => {
        try {
          const capabilities =
            (await DataSource(httpClient).getDriverCapabilities(
              driver ? driver.kind : ''
            )) ?? Feature.NotImplemented;

          return { driver, capabilities };
        } catch (err) {
          /**
           * Instead of erroring out if one of DC agents is unreachable, set the unreachable one to {} and let the request pass.
           */
          return { driver, capabilities: {} };
        }
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
