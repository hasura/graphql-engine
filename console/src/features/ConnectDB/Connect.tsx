import React from 'react';

import { Button } from '@/new-components/Button';
import { Form, InputField, Select } from '@/new-components/Form';
import { IndicatorCard } from '@/new-components/IndicatorCard';

import { Configuration } from './components/Configuration';
import { useLoadSchema, useSubmit } from './hooks';

export const Connect = () => {
  const {
    data: { schemas, drivers },
    isLoading,
    isError,
  } = useLoadSchema();

  const { submit, isLoading: submitIsLoading } = useSubmit();

  if (isError) {
    return (
      <IndicatorCard status="negative">
        Error loading connection schemas
      </IndicatorCard>
    );
  }

  if (isLoading) {
    return <IndicatorCard>Loading</IndicatorCard>;
  }

  if (!schemas) {
    return (
      <IndicatorCard>
        Unable to retrieve any valid configuration settings
      </IndicatorCard>
    );
  }

  if (!drivers) {
    return <IndicatorCard>Unable to load drivers</IndicatorCard>;
  }

  return (
    <Form
      schema={schemas}
      onSubmit={submit}
      options={{
        defaultValues: {
          name: '',
          driver: 'postgres',
        },
      }}
    >
      {options => {
        return (
          <div>
            <InputField
              type="text"
              placeholder="Enter a display name"
              name="name"
              label="Display Name"
            />
            <Select
              name="driver"
              label="Driver"
              options={drivers.map(driver => ({
                value: driver,
                label: driver,
              }))}
            />
            <div className="max-w-xl">
              <p className="flex items-center font-semibold text-gray-600 mb-xs">
                Configuration
              </p>
              <Configuration name="configuration" />
            </div>
            <Button type="submit" mode="primary" isLoading={submitIsLoading}>
              Connect Database
            </Button>
            {!!Object(options.formState.errors)?.keys?.length && (
              <div className="mt-6 max-w-xl">
                <IndicatorCard status="negative">
                  Error submitting form, see error messages above
                </IndicatorCard>
              </div>
            )}
          </div>
        );
      }}
    </Form>
  );
};
