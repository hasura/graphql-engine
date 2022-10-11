import { CustomizationForm } from '@/features/ConnectDB';
import { Button } from '@/new-components/Button';
import { Forms, InputField } from '@/new-components/Form';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import React from 'react';
import { Configuration } from './components/Configuration';
import { Driver } from './components/Driver';
import { EditConnection } from './EditConnection';
import { useLoadSchema, useSubmit } from './hooks';

interface Props {
  name: string;
  driver: string;
  onDriverChange: (driver: string, name: string) => void;
}

const CreateConnection = ({ name, driver, onDriverChange }: Props) => {
  const {
    data: { schema, drivers, defaultValues },
    isLoading,
    isError,
  } = useLoadSchema({
    name,
    driver,
  });

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

  if (!schema) {
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
    <Forms.New
      key={`${defaultValues.name}-${defaultValues.driver}` || 'new-connection'}
      schema={schema}
      onSubmit={submit}
      options={{
        defaultValues,
      }}
      className="p-0 pl-sm"
    >
      {options => {
        return (
          <div>
            <InputField type="text" name="name" label="Database Display Name" />

            <Driver onDriverChange={onDriverChange} />

            <div className="max-w-xl">
              <Configuration name="configuration" />
            </div>
            <div className="mt-4">
              <CustomizationForm />
            </div>
            <Button
              type="submit"
              className="mt-4"
              mode="primary"
              isLoading={submitIsLoading}
            >
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
    </Forms.New>
  );
};

export const Connect = {
  CreateConnection,
  EditConnection,
};
