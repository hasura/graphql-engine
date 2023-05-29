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
import { useEffect, useState } from 'react';
import { LimitedFeatureWrapper } from '../LimitedFeatureWrapper/LimitedFeatureWrapper';
import { DynamicDBRouting } from './parts/DynamicDBRouting';
import { Tabs } from '../../../../new-components/Tabs';
import { DisplayToastErrorMessage } from '../Common/DisplayToastErrorMessage';

interface ConnectPostgresWidgetProps {
  dataSourceName?: string;

  //overrides for pg like sources;
  overrideDriver?: string;
  overrideDisplayName?: string;
}

export const ConnectPostgresWidget = (props: ConnectPostgresWidgetProps) => {
  const { dataSourceName, overrideDriver, overrideDisplayName } = props;
  const [tab, setTab] = useState('connectionDetails');

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
          title: err.name,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });

  const handleSubmit = (formValues: PostgresConnectionSchema) => {
    const payload = generatePostgresRequestPayload({
      driver: overrideDriver ?? 'postgres',
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
          'Error while retriving database. Please check if the database is of type postgres',
      });
    }
  }, [metadataSource, reset]);

  const hiddenOptions =
    overrideDriver === 'cockroach' ? ['connectionParams'] : [];

  const dynamicDBRoutingTab =
    dataSourceName && isEditMode && window.__env.consoleType !== 'oss'
      ? [
          {
            value: 'dynamicDBRouting',
            label: 'Dynamic Routing',
            content: (
              <div className="mt-sm">
                <LimitedFeatureWrapper
                  id="dynamic-db-routing"
                  title="Dynamic Routing for Databases"
                  description="Effortlessly scale your data architecture with Dynamic Routing for databases, allowing you to easily route GraphQL requests to different database connections and leverage different database topology patterns with Hasura."
                >
                  <DynamicDBRouting sourceName={dataSourceName} />
                </LimitedFeatureWrapper>
              </div>
            ),
          },
        ]
      : [];

  return (
    <>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${overrideDisplayName ?? 'Postgres'} Connection`
          : `Connect ${overrideDisplayName ?? 'Postgres'} Database`}
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

                  <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
                    <DatabaseUrl
                      name="configuration.connectionInfo.databaseUrl"
                      hideOptions={hiddenOptions}
                    />
                  </div>

                  <div className="mt-sm">
                    <Collapsible
                      triggerChildren={
                        <div className="font-semibold text-muted">
                          Advanced Settings
                        </div>
                      }
                    >
                      <PoolSettings
                        name={`configuration.connectionInfo.poolSettings`}
                      />
                      <IsolationLevel
                        name={`configuration.connectionInfo.isolationLevel`}
                      />
                      <UsePreparedStatements
                        name={`configuration.connectionInfo.usePreparedStatements`}
                      />
                      <ExtensionSchema name="configuration.extensionSchema" />
                      <LimitedFeatureWrapper
                        title="Looking to add SSL Settings?"
                        id="db-ssl-settings"
                        description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
                      >
                        <div className="mt-sm">
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
                        </div>
                      </LimitedFeatureWrapper>
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
                      id="read-replicas"
                      title="Improve performance and handle increased traffic with read replicas"
                      description="Scale your database by offloading read queries to
            read-only replicas, allowing for better performance
            and availability for users."
                    >
                      <Collapsible
                        triggerChildren={
                          <div className="font-semibold text-muted">
                            Read Replicas
                          </div>
                        }
                      >
                        <ReadReplicas
                          name="configuration.readReplicas"
                          hideOptions={hiddenOptions}
                        />
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
          ...dynamicDBRoutingTab,
        ]}
      />
    </>
  );
};
