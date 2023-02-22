import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { Tabs } from '../../../../new-components/Tabs';
import { Button } from '../../../../new-components/Button';
import { useEffect, useState } from 'react';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { Configuration } from './parts/Configuration';
import { getDefaultValues, BigQueryConnectionSchema, schema } from './schema';
import { get } from 'lodash';
import { FaExclamationTriangle } from 'react-icons/fa';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useMetadata } from '../../../hasura-metadata-api';
import { generatePostgresRequestPayload } from './utils/generateRequests';

interface ConnectBigQueryWidgetProps {
  dataSourceName?: string;
}

export const ConnectBigQueryWidget = (props: ConnectBigQueryWidgetProps) => {
  const { dataSourceName } = props;

  const isEditMode = !!dataSourceName;

  const { data: metadataSource } = useMetadata(m =>
    m.metadata.sources.find(source => source.name === dataSourceName)
  );

  const { createConnection, editConnection, isLoading } =
    useManageDatabaseConnection({
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: isEditMode
            ? 'Database updated successful!'
            : 'Database added successfully!',
        });
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: 'Error while adding database',
          children: JSON.stringify(err),
        });
      },
    });

  const handleSubmit = (formValues: BigQueryConnectionSchema) => {
    const payload = generatePostgresRequestPayload({
      driver: 'bigquery',
      values: formValues,
    });

    if (isEditMode) {
      editConnection(payload);
    } else {
      createConnection(payload);
    }
  };

  const [tab, setTab] = useState('connection_details');
  const {
    Form,
    methods: { formState, reset },
  } = useConsoleForm({
    schema,
  });

  useEffect(() => {
    try {
      reset(getDefaultValues(metadataSource));
    } catch (err) {
      hasuraToast({
        type: 'error',
        title:
          'Error while retriving database. Please check if the database is of type postgres',
      });
    }
  }, [metadataSource, reset]);

  const connectionDetailsTabErrors = [
    get(formState.errors, 'name'),
    get(formState.errors, 'configuration.connectionInfo'),
  ].filter(Boolean);

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? 'Edit BigQuery Connection'
          : 'Connect New BigQuery Database'}
      </div>
      <Form onSubmit={handleSubmit}>
        <Tabs
          value={tab}
          onValueChange={value => setTab(value)}
          items={[
            {
              value: 'connection_details',
              label: 'Connection Details',
              icon: connectionDetailsTabErrors.length ? (
                <FaExclamationTriangle className="text-red-800" />
              ) : undefined,
              content: (
                <div className="mt-sm">
                  <InputField
                    name="name"
                    label="Database display name"
                    placeholder="Database name"
                  />
                  <Configuration name="configuration" />
                </div>
              ),
            },
            {
              value: 'customization',
              label: 'GraphQL Customization',
              content: <GraphQLCustomization name="customization" />,
            },
          ]}
        />
        <div className="flex justify-end">
          <Button
            type="submit"
            mode="primary"
            isLoading={isLoading}
            loadingText="Saving"
          >
            {isEditMode ? 'Update Connection' : 'Connect Database'}
          </Button>
        </div>
      </Form>
    </div>
  );
};
