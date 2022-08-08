import axios from 'axios';
import { useQuery } from 'react-query';

import { DataSource } from '@/features/DataSource';

const fetch = axios.create();
const dataSourceFetch = DataSource(fetch);

const useAvailableDrivers = () => {
  return useQuery({
    queryKey: ['getDrivers'],
    queryFn: async () => {
      return dataSourceFetch.driver.getSupportedDrivers();
    },
  });
};

const usePossibleFormSchemas = () => {
  return useQuery({
    queryKey: ['validation-schemas'],
    queryFn: async () => {
      return dataSourceFetch.connectDB.getFormSchema();
    },
  });
};

export const useLoadSchema = () => {
  const {
    data: schemas,
    isLoading: schemasLoading,
    isError: isSchemaError,
    error: schemaError,
  } = usePossibleFormSchemas();
  const {
    data: drivers,
    isLoading: driversLoading,
    isError: isDriversError,
    error: driversError,
  } = useAvailableDrivers();

  const isLoading = schemasLoading || driversLoading;
  const isError = isSchemaError || isDriversError;
  const error = schemaError || driversError;

  return { data: { schemas, drivers }, isLoading, isError, error };
};
