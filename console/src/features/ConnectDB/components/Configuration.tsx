import React from 'react';
import axios from 'axios';
import { useQuery } from 'react-query';
import { useFormContext } from 'react-hook-form';

import { DataSource, SupportedDrivers, Feature } from '@/features/DataSource';
import { IndicatorCard } from '@/new-components/IndicatorCard';

import { Field } from './Fields';

const useConfigSchema = (driver: SupportedDrivers) => {
  const fetch = axios.create();
  return useQuery({
    queryKey: [driver, 'configSchema'],
    queryFn: async () => {
      return DataSource(fetch).connectDB.getConfigSchema(driver);
    },
    enabled: !!driver,
  });
};

interface Props {
  name: string;
}

export const Configuration = ({ name }: Props) => {
  const { watch } = useFormContext();
  const driver: SupportedDrivers = watch('driver');
  const { data: schema, isLoading, isError } = useConfigSchema(driver);

  if (isError)
    return (
      <IndicatorCard status="negative">
        Error loading driver configuration
      </IndicatorCard>
    );

  if (schema === Feature.NotImplemented) {
    return <IndicatorCard>Feature is not available for {driver}</IndicatorCard>;
  }

  if (!driver) {
    return <IndicatorCard>Driver not selected</IndicatorCard>;
  }

  if (isLoading) {
    return <IndicatorCard>Loading configuration info...</IndicatorCard>;
  }

  if (!schema || schema.configSchema.type !== 'object')
    return (
      <IndicatorCard status="negative">
        Unable to find a valid schema for the {driver}
      </IndicatorCard>
    );

  return (
    <>
      {Object.entries(schema.configSchema.properties).map(([key, value]) => (
        <Field
          key={key}
          property={value}
          otherSchemas={schema.otherSchemas}
          name={`${name}.${key}`}
        />
      ))}
    </>
  );
};
