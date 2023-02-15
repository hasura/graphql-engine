import { InputField, useConsoleForm } from '@/new-components/Form';
import { Tabs } from '@/new-components/Tabs';
import { Button } from '@/new-components/Button';
import { useEffect, useState } from 'react';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { Configuration } from './parts/Configuration';
import { getDefaultValues, PostgresConnectionSchema, schema } from './schema';
import { ReadReplicas } from './parts/ReadReplicas';
import { get } from 'lodash';
import { FaExclamationTriangle } from 'react-icons/fa';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { hasuraToast } from '@/new-components/Toasts';
import { useMetadata } from '@/features/hasura-metadata-api';
import { generatePostgresRequestPayload } from './utils/generateRequests';
import { isProConsole } from '@/utils';

interface ConnectPostgresWidgetProps {
  dataSourceName?: string;

  //overrides for pg like sources;
  overrideDriver?: string;
  overrideDisplayName?: string;
}

export const ConnectPostgresWidget = (props: ConnectPostgresWidgetProps) => {
  const { dataSourceName, overrideDriver, overrideDisplayName } = props;

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
            ? 'Database updated successfully!'
            : 'Database added successfully!',
        });
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: `Error while ${isEditMode ? 'updating' : 'adding'} database`,
          children: JSON.stringify(err),
        });
      },
    });

  const handleSubmit = (formValues: PostgresConnectionSchema) => {
    const payload = generatePostgresRequestPayload({
      driver: overrideDriver ?? 'postgres',
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
    methods: { formState, watch, reset },
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

  const readReplicas = watch('configuration.readReplicas');

  const connectionDetailsTabErrors = [
    get(formState.errors, 'name'),
    get(formState.errors, 'configuration.connectionInfo'),
    get(formState.errors, 'configuration.extensionSchema'),
  ].filter(Boolean);

  const readReplicasError = [
    get(formState.errors, 'configuration.readReplicas'),
  ].filter(Boolean);

  const hiddenOptions =
    overrideDriver === 'cockroach' ? ['connectionParams'] : [];

  const proConsoleTabs = isProConsole(window.__env)
    ? [
        {
          value: 'read_replicas',
          label: `Read Replicas ${
            readReplicas?.length ? `(${readReplicas.length})` : ''
          }`,
          icon: readReplicasError.length ? (
            <FaExclamationTriangle className="text-red-800" />
          ) : undefined,
          content: (
            <ReadReplicas
              name="configuration.readReplicas"
              hideOptions={hiddenOptions}
            />
          ),
        },
      ]
    : [];

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${overrideDisplayName ?? 'Postgres'} Connection`
          : `Connect New ${overrideDisplayName ?? 'Postgres'} Database`}
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
                  <Configuration
                    name="configuration"
                    hideOptions={hiddenOptions}
                  />
                </div>
              ),
            },
            ...proConsoleTabs,
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
