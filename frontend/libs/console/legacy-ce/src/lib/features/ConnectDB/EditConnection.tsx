import {
  Button,
  InputField,
  SelectField,
  useConsoleForm,
  IndicatorCard,
} from '@hasura/shared/ui';
import { useEffect } from 'react';
import { z } from 'zod';
import { Configuration } from './components/Configuration';
import {
  useEditDataSourceConnection,
  useEditDataSourceConnectionInfo,
} from './hooks';
import { CustomizationForm } from './components/Customization';

export const EditConnection = () => {
  const { data, isLoading } = useEditDataSourceConnectionInfo();
  const { schema, name, driver, configuration, customization } = data || {};
  const { submit, isPending: submitIsLoading } = useEditDataSourceConnection();
  const {
    methods: { formState, reset },
    Form,
  } = useConsoleForm({
    schema: schema || z.any(),
  });

  useEffect(() => {
    if (data) {
      reset({
        name,
        driver,
        configuration: (configuration as any)?.value,
        replace_configuration: true,
        customization,
      });
    }
  }, [configuration, customization, data, driver, name, reset]);

  if (isLoading) return <>Loading...</>;

  if (!data) return <>Error</>;

  if (!schema) return <>Could not find schema</>;

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold p-4">
        Edit {name} database connection
      </div>
      <Form onSubmit={submit} className="p-0 pl-sm">
        <div className="max-w-5xl">
          <InputField name="name" label="Database Display Name" />

          <SelectField
            options={driver ? [{ label: driver, value: driver }] : []}
            name="driver"
            label="Data Source Driver"
            disabled
          />

          <div className="max-w-xl">
            <Configuration name="configuration" />
          </div>
          <div className="mt-4">
            <CustomizationForm />
          </div>
          <div className="mt-4">
            <Button type="submit" mode="primary" loading={submitIsLoading}>
              Edit Connection
            </Button>
          </div>

          {!!Object(formState.errors)?.keys?.length && (
            <div className="mt-6 max-w-xl">
              <IndicatorCard status="negative">
                Error submitting form, see error messages above
              </IndicatorCard>
            </div>
          )}
        </div>
      </Form>
    </div>
  );
};
