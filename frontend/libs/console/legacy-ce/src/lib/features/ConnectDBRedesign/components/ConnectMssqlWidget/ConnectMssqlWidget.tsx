import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { useEffect } from 'react';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { getDefaultValues, MssqlConnectionSchema, schema } from './schema';
import { ReadReplicas } from './parts/ReadReplicas';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useMetadata } from '../../../hasura-metadata-api';
import { generateMssqlRequestPayload } from './utils/generateRequests';
import { ConnectionString } from './parts/ConnectionString';
import { areReadReplicasEnabled } from '../ConnectPostgresWidget/utils/helpers';
import { Collapsible } from '../../../../new-components/Collapsible';
import { PoolSettings } from './parts/PoolSettings';

interface ConnectMssqlWidgetProps {
  dataSourceName?: string;
}

export const ConnectMssqlWidget = (props: ConnectMssqlWidgetProps) => {
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

  const handleSubmit = (formValues: MssqlConnectionSchema) => {
    const payload = generateMssqlRequestPayload({
      driver: 'mssql',
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
          'Error while retriving database. Please check if the database is of type mssql',
      });
    }
  }, [metadataSource, reset]);

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode ? 'Edit MSSQL Connection' : 'Connect MSSQL Database'}
      </div>
      <Form onSubmit={handleSubmit}>
        <InputField
          name="name"
          label="Database name"
          placeholder="Database name"
        />
        <ConnectionString name="configuration.connectionInfo.connectionString" />

        <div className="mt-sm">
          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">Advanced Settings</div>
            }
          >
            <PoolSettings name="configuration.connectionInfo.poolSettings" />
          </Collapsible>
        </div>

        {areReadReplicasEnabled() && (
          <div className="mt-sm">
            <Collapsible
              triggerChildren={
                <div className="font-semibold text-muted">Read Replicas</div>
              }
            >
              <ReadReplicas name="configuration.readReplicas" />
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
