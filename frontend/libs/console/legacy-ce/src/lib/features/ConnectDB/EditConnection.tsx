import { CustomizationForm } from '.';
import { Button } from '../../new-components/Button';
import { InputField, Select, useConsoleForm } from '../../new-components/Form';
import { IndicatorCard } from '../../new-components/IndicatorCard';
import React, { useEffect } from 'react';
import { useQuery } from 'react-query';
import { z } from 'zod';
import { useTableDefinition } from '../Data';
import { DataSource, exportMetadata } from '../DataSource';
import { useHttpClient } from '../Network';
import { Configuration } from './components/Configuration';
import { useEditDataSourceConnection } from './hooks';

const useEditDataSourceConnectionInfo = () => {
  const httpClient = useHttpClient();
  const urlData = useTableDefinition(window.location);

  return useQuery({
    queryKey: ['edit-connection'],
    queryFn: async () => {
      if (urlData.querystringParseResult === 'error')
        throw Error('Something went wrong while parsing the URL parameters');
      const { database: dataSourceName } = urlData.data;
      const { metadata } = await exportMetadata({ httpClient });

      if (!metadata) throw Error('Unavailable to fetch metadata');

      const metadataSource = metadata.sources.find(
        source => source.name === dataSourceName
      );

      if (!metadataSource) throw Error('Unavailable to fetch metadata source');

      const schema = await DataSource(httpClient).connectDB.getFormSchema(
        metadataSource.kind
      );

      return {
        schema,
        configuration: metadataSource.configuration,
        driver: metadataSource.kind,
        name: metadataSource.name,
        customization: metadataSource.customization,
      };
    },
  });
};

export const EditConnection = () => {
  const { data, isLoading } = useEditDataSourceConnectionInfo();
  const {
    schema = z.any(),
    name,
    driver,
    configuration,
    customization,
  } = data || {};
  const { submit, isLoading: submitIsLoading } = useEditDataSourceConnection();
  const {
    methods: { formState, reset },
    Form,
  } = useConsoleForm({
    schema,
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
          <InputField type="text" name="name" label="Database Display Name" />

          <Select
            options={[{ label: driver || '', value: driver }]}
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
            <Button type="submit" mode="primary" isLoading={submitIsLoading}>
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
