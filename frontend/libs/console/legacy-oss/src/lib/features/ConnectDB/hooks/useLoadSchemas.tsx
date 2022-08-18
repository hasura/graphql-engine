import axios from 'axios';
import { useQueries } from 'react-query';

import { DataSource } from '@/features/DataSource';
import { useDefaultValues } from './useDefaultValues';

const fetch = axios.create();
const dataSourceFetch = DataSource(fetch);

const possibleFormSchemasQuery = {
  queryKey: ['validation-schemas'],
  queryFn: async () => dataSourceFetch.connectDB.getFormSchema(),
};

const availableDriversQuery = {
  queryKey: ['getDrivers'],
  queryFn: async () => dataSourceFetch.driver.getSupportedDrivers(),
};

interface Args {
  name: string;
  driver: string;
}

export const useLoadSchema = ({ name, driver }: Args) => {
  const results = useQueries([possibleFormSchemasQuery, availableDriversQuery]);

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

  const [schemasResult, driversResult] = results;

  const schemas = schemasResult.data;
  const drivers = driversResult.data;

  const error = results.some(result => result.error) || defaultValuesError;
  return {
    data: { schemas, drivers, defaultValues },
    isLoading,
    isError,
    error,
  };
};
