import { useEffect, useState } from 'react';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { getDefaultValues, MssqlConnectionSchema, schema } from './schema';
import { ReadReplicas } from './parts/ReadReplicas';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import {
  hasuraToast,
  InputField,
  useConsoleForm,
  Button,
  Collapsible,
  Tabs,
  DisplayToastErrorMessage,
  Text,
} from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import { generateMssqlRequestPayload } from './utils/generateRequests';
import { ConnectionString } from './parts/ConnectionString';
import { PoolSettings } from './parts/PoolSettings';
import { LimitedFeatureWrapper } from '../LimitedFeatureWrapper/LimitedFeatureWrapper';
import { Flex, Heading } from '@radix-ui/themes';

interface ConnectMssqlWidgetProps {
  dataSourceName?: string;
}

export const ConnectMssqlWidget = (props: ConnectMssqlWidgetProps) => {
  const { dataSourceName } = props;

  const isEditMode = !!dataSourceName;
  const [tab, setTab] = useState('connectionDetails');

  const { data: metadataSource } = useMetadata((m) =>
    m.metadata.sources.find((source) => source.name === dataSourceName),
  );

  const { createConnection, editConnection, isPending } =
    useManageDatabaseConnection({
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: isEditMode
            ? 'Database updated successful!'
            : 'Database added successfully!',
        });
      },
      onError: (err) => {
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
      <Heading size="4">
        {isEditMode ? 'Edit MSSQL Connection' : 'Connect MSSQL Database'}
      </Heading>

      <Tabs
        value={tab}
        onValueChange={(value) => setTab(value)}
        items={[
          {
            value: 'connectionDetails',
            label: 'Connection Details',
            content: (
              <div className="mt-4">
                <Form onSubmit={handleSubmit}>
                  <InputField
                    name="name"
                    label="Database name"
                    fieldProps={{
                      placeholder: 'Database name',
                    }}
                  />
                  <ConnectionString name="configuration.connectionInfo.connectionString" />

                  <div className="mt-4">
                    <Collapsible
                      triggerChildren={
                        <Text weight="bold">Advanced Settings</Text>
                      }
                    >
                      <PoolSettings name="configuration.connectionInfo.poolSettings" />
                    </Collapsible>
                  </div>

                  <div className="mt-4">
                    <Collapsible
                      triggerChildren={
                        <Text weight="bold">GraphQL Customization</Text>
                      }
                    >
                      <GraphQLCustomization name="customization" />
                    </Collapsible>
                  </div>

                  <div className="mt-4">
                    <LimitedFeatureWrapper
                      title="Looking to add Read Replicas?"
                      id="read-replicas"
                      description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
                    >
                      <Collapsible
                        triggerChildren={
                          <Text weight="bold">Read Replicas</Text>
                        }
                      >
                        <ReadReplicas name="configuration.readReplicas" />
                      </Collapsible>
                    </LimitedFeatureWrapper>
                  </div>

                  <Flex justify="end" className="mt-4">
                    <Button
                      type="submit"
                      mode="primary"
                      loading={isPending}
                      loadingText="Saving"
                    >
                      {isEditMode ? 'Update Connection' : 'Connect Database'}
                    </Button>
                  </Flex>
                </Form>
              </div>
            ),
          },
        ]}
      />
    </div>
  );
};
