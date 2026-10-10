import { SupportedDriver } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery } from '@tanstack/react-query';
import { getDatabaseMethods, NotImplementedError } from '../../driver';

export const useDatabaseConfiguration = (driver: SupportedDriver) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: ['configSchema', driver],
    queryFn: async () => {
      const databaseMethods = getDatabaseMethods(driver);
      if (!databaseMethods.introspection?.getDatabaseConfiguration) {
        throw new NotImplementedError();
      }

      return databaseMethods.introspection.getDatabaseConfiguration({
        driver,
        endpoints,
        fetchJson,
      });
    },
    enabled: !!driver,
    staleTime: Infinity,
  });
};
