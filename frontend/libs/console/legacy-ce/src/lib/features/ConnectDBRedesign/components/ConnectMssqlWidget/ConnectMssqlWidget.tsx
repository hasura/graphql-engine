import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { useEffect, useState } from 'react';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { getDefaultValues, MssqlConnectionSchema, schema } from './schema';
import { ReadReplicas } from './parts/ReadReplicas';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useMetadata } from '../../../hasura-metadata-api';
import { generateMssqlRequestPayload } from './utils/generateRequests';
import { ConnectionString } from './parts/ConnectionString';
import { Collapsible } from '../../../../new-components/Collapsible';
import { PoolSettings } from './parts/PoolSettings';
import { LimitedFeatureWrapper } from '../LimitedFeatureWrapper/LimitedFeatureWrapper';
import { Tabs } from '../../../../new-components/Tabs';
import { DisplayToastErrorMessage } from '../Common/DisplayToastErrorMessage';

interface ConnectMssqlWidgetProps {
  dataSourceName?: string;
}

export const ConnectMssqlWidget = (props: ConnectMssqlWidgetProps) => {
  const { dataSourceName } = props;

  const isEditMode = !!dataSourceName;
  const [tab, setTab] = useState('connectionDetails');

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
          title: err.name,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });

  const handleSubmit = (formValues: MssqlConnectionSchema) => {
    const payload = generateMssqlRequestPayload({
      driver: 'mssql',
      values: formValues,
    });

    if (isEditMode) {
      editConnection({ originalName: dataSourceName, ...payload });
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

      <Tabs
        value={tab}
        onValueChange={value => setTab(value)}
        items={[
          {
            value: 'connectionDetails',
            label: 'Connection Details',
            content: (
              <div className="mt-sm">
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
                        <div className="font-semibold text-muted">
                          Advanced Settings
                        </div>
                      }
                    >
                      <PoolSettings name="configuration.connectionInfo.poolSettings" />
                    </Collapsible>
                  </div>

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

                  <div className="mt-sm">
                    <LimitedFeatureWrapper
                      title="Looking to add Read Replicas?"
                      id="read-replicas"
                      description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
                    >
                      <Collapsible
                        triggerChildren={
                          <div className="font-semibold text-muted">
                            Read Replicas
                          </div>
                        }
                      >
                        <ReadReplicas name="configuration.readReplicas" />
                      </Collapsible>
                    </LimitedFeatureWrapper>
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
            ),
          },
        ]}
      />
    </div>
  );
};
