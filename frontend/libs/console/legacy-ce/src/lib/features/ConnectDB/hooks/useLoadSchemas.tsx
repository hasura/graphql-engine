import { useQueries } from 'react-query';
import { z } from 'zod';
import { useHttpClient } from '../../Network';
import { DataSource } from '../../DataSource';
import { useDefaultValues } from './useDefaultValues';

interface Args {
  name: string;
  driver: string;
}

export const useLoadSchema = ({ name, driver }: Args) => {
  const httpClient = useHttpClient();
  const results = useQueries([
    {
      queryKey: ['validation-schema', driver],
      queryFn: async () =>
        DataSource(httpClient).connectDB.getFormSchema(driver),
    },
    {
      queryKey: ['getDrivers'],
      queryFn: async () => DataSource(httpClient).driver.getAllSourceKinds(),
    },
  ]);

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
    results.some(result => result.isLoading) || defaultValuesIsLoading;
  const isError =
    results.some(result => result.isError) || defaultValuesIsError;

  const [schemaResult, driversResult] = results;

  const schema = schemaResult.data || z.any();
  const drivers = driversResult.data;

  const error = results.some(result => result.error) || defaultValuesError;
  return {
    data: { schema, drivers, defaultValues },
    isLoading,
    isError,
    error,
  };
};
