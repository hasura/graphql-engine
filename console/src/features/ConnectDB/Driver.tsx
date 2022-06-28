import { useQuery } from 'react-query';
import { DataSource } from '@/features/DataSource';
import React from 'react';
import axios from 'axios';
import { Select } from '@/new-components/Form';

const useAvailableDrivers = () => {
  const fetch = axios.create();
  return useQuery({
    queryKey: ['getDrivers'],
    queryFn: async () => {
      return DataSource(fetch).driver.getSupportedDrivers();
    },
  });
};

export const Driver = ({ name }: { name: string }) => {
  const { data: drivers, isLoading, isError } = useAvailableDrivers();

  if (isLoading) return <>Fetching driver info...</>;

  if (isError) return <>Something went wrong while fetching drivers</>;

  if (!drivers) return <>No available drivers were found</>;

  return (
    <div className="py-4">
      <Select
        options={drivers.map(driver => ({ value: driver, label: driver }))}
        name={name}
        label="Driver"
      />
    </div>
  );
};
