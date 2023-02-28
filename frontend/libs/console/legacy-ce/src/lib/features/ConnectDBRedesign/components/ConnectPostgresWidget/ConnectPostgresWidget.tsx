import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { getDefaultValues, PostgresConnectionSchema, schema } from './schema';
import { ReadReplicas } from './parts/ReadReplicas';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useMetadata } from '../../../hasura-metadata-api';
import { generatePostgresRequestPayload } from './utils/generateRequests';
import { DatabaseUrl } from './parts/DatabaseUrl';
import { PoolSettings } from './parts/PoolSettings';
import { IsolationLevel } from './parts/IsolationLevel';
import { UsePreparedStatements } from './parts/UsePreparedStatements';
import { SslSettings } from './parts/SslSettings';
import { Collapsible } from '../../../../new-components/Collapsible';
import { ExtensionSchema } from './parts/ExtensionSchema';
import { areReadReplicasEnabled, areSSLSettingsEnabled } from './utils/helpers';
import { useEffect } from 'react';

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

  const {
    Form,
    methods: { reset },
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

  const hiddenOptions =
    overrideDriver === 'cockroach' ? ['connectionParams'] : [];

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${overrideDisplayName ?? 'Postgres'} Connection`
          : `Connect ${overrideDisplayName ?? 'Postgres'} Database`}
      </div>
      <Form onSubmit={handleSubmit}>
        <InputField
          name="name"
          label="Database name"
          placeholder="Database name"
        />

        <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
          <DatabaseUrl
            name="configuration.connectionInfo.databaseUrl"
            hideOptions={hiddenOptions}
          />
        </div>

        <div className="mt-sm">
          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">Advanced Settings</div>
            }
          >
            <PoolSettings name={`configuration.connectionInfo.poolSettings`} />
            <IsolationLevel
              name={`configuration.connectionInfo.isolationLevel`}
            />
            <UsePreparedStatements
              name={`configuration.connectionInfo.usePreparedStatements`}
            />
            <ExtensionSchema name="configuration.extensionSchema" />
            {areSSLSettingsEnabled() && (
              <Collapsible
                triggerChildren={
                  <div className="font-semibold text-muted">
                    SSL Certificates Settings
                    <span className="px-1.5 italic font-light">
                      (Certificates will be loaded from{' '}
                      <a href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database">
                        environment variables
                      </a>
                      )
                    </span>
                  </div>
                }
              >
                <SslSettings
                  name={`configuration.connectionInfo.sslSettings`}
                />
              </Collapsible>
            )}
          </Collapsible>
        </div>

        {areReadReplicasEnabled() && (
          <div className="mt-sm">
            <Collapsible
              triggerChildren={
                <div className="font-semibold text-muted">Read Replicas</div>
              }
            >
              <ReadReplicas
                name="configuration.readReplicas"
                hideOptions={hiddenOptions}
              />
            </Collapsible>
          </div>
        )}

        <div className="mt-sm">
          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">
                GraphQL Customization
              </div>
            }
          >
            <GraphQLCustomization name="customization" />
          </Collapsible>
        </div>

        <div className="flex justify-end mt-sm">
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
