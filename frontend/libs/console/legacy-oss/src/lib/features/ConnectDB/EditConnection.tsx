import { Button } from '@/new-components/Button';
import { Form, InputField, Select } from '@/new-components/Form';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import React from 'react';
import { useQuery } from 'react-query';
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
      };
    },
  });
};

export const EditConnection = () => {
  const { data, isLoading } = useEditDataSourceConnectionInfo();
  const { submit, isLoading: submitIsLoading } = useEditDataSourceConnection();

  if (isLoading) return <>Loading...</>;

  if (!data) return <>Error</>;

  const { schema, name, driver, configuration } = data;

  if (!schema) return <>Could not find schema</>;

  return (
    <Form
      schema={schema}
      onSubmit={values => {
        submit(values);
      }}
      options={{
        defaultValues: {
          name,
          driver,
          configuration: (configuration as any).value,
          replace_configuration: true,
        },
      }}
      className="p-0 pl-sm"
    >
      {options => {
        return (
          <div>
            <InputField type="text" name="name" label="Database Display Name" />

            <Select
              options={[{ label: driver, value: driver }]}
              name="driver"
              label="Data Source Driver"
              disabled
            />

            <div className="max-w-xl">
              <Configuration name="configuration" />
            </div>
            <Button type="submit" mode="primary" isLoading={submitIsLoading}>
              Edit Connection
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
