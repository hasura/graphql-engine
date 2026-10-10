import { useQueries } from '@tanstack/react-query';
import { z } from 'zod';
import {
  getAllSourceKinds,
  getConnectDatabaseFormSchema,
} from '@hasura/metadata/data-source';
import { useDefaultValues } from './useDefaultValues';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { SupportedDriver } from '@hasura/shared/types';

interface Args {
  name: string;
  driver: SupportedDriver;
}

export const useLoadSchema = ({ name, driver }: Args) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const results = useQueries({
    queries: [
      {
        queryKey: [driver, 'validation-schema'],
        queryFn: async () => {
          return getConnectDatabaseFormSchema({
            driver,
            endpoints,
            fetchJson,
          });
        },
      },
      {
        queryKey: ['getDrivers'],
        queryFn: () =>
          getAllSourceKinds({
            endpoints,
            fetchJson,
          }),
      },
    ],
  });

  // get default values if existing connection info is passed in
  // it would be nice to do this as part of the useQueries array above
  // but currently not possible because of the way we fetch metadata
  const {
    data: defaultValues,
    isLoading: defaultValuesIsLoading,
    isError: defaultValuesIsError,
    error: defaultValuesError,
  } = useDefaultValues({ name, driver });

  const isLoading =
    results.some((result) => result.isLoading) || defaultValuesIsLoading;
  const isError =
    results.some((result) => result.isError) || defaultValuesIsError;

  const [schemaResult, driversResult] = results;

  const schema = schemaResult.data || z.any();
  const drivers = driversResult.data;

  const error = results.some((result) => result.error) || defaultValuesError;
  return {
    data: { schema, drivers, defaultValues },
    isLoading,
    isError,
    error,
  };
};
