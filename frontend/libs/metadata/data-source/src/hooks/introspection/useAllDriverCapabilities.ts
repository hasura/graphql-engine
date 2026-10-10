import { UseQueryOptions, useQuery } from '@tanstack/react-query';
import { Capabilities } from '@hasura/dc-api-types';
import { useMetadata } from '@hasura/metadata/api';
import { SupportedDriver } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { HttpError } from '@hasura/shared/types';
import { getDatabaseMethods } from '../../driver';

type AllCapabilitiesReturnType = {
  driver: SupportedDriver;
  capabilities?: Capabilities;
}[];

export const useAllDriverCapabilities = <
  FinalResult = AllCapabilitiesReturnType,
>(
  options?: Omit<
    UseQueryOptions<AllCapabilitiesReturnType, HttpError, FinalResult>,
    'queryKey'
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const { data: sources, isFetching: isFetchingMetadata } = useMetadata(
    (m) => m.metadata.sources,
  );

  return useQuery<AllCapabilitiesReturnType, HttpError, FinalResult>({
    queryKey: ['all_capabilities'],
    queryFn: async () => {
      if (!sources?.length) {
        return [];
      }

      const drivers = [...new Set(sources.map((source) => source.kind))];
      const result = await Promise.all(
        drivers.map(async (driver) => {
          try {
            const databaseMethods = getDatabaseMethods(driver);
            if (!databaseMethods.introspection.getDriverCapabilities) {
              return null;
            }

            return databaseMethods.introspection
              .getDriverCapabilities({
                endpoints,
                fetchJson,
                driver: driver,
              })
              .then((capabilities) => ({
                driver,
                capabilities,
              }));
          } catch (err) {
            /**
             * Instead of erroring out if one of DC agents is unreachable, set the unreachable one to {} and let the request pass.
             */
            return null;
          }
        }),
      ).then((caps) => caps.filter(Boolean));

      return result as AllCapabilitiesReturnType;
    },
    enabled: !isFetchingMetadata && options?.enabled,
    ...options,
  });
};
