import { useEffect, useState } from 'react';
import {
  Button,
  InputField,
  useConsoleForm,
  Collapsible,
  Tabs,
  hasuraToast,
  DisplayToastErrorMessage,
} from '@hasura/shared/ui';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { Configuration } from './parts/Configuration';
import { getDefaultValues, BigQueryConnectionSchema, schema } from './schema';
import { useMetadata } from '@hasura/metadata/api';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { generateBigQueryRequestPayload } from './utils/generateRequests';
import { Flex, Heading } from '@radix-ui/themes';

interface ConnectBigQueryWidgetProps {
  dataSourceName?: string;
}

export const ConnectBigQueryWidget = (props: ConnectBigQueryWidgetProps) => {
  const { dataSourceName } = props;

  const isEditMode = !!dataSourceName;

  const { data: metadataSource } = useMetadata((m) =>
    m.metadata.sources.find((source) => source.name === dataSourceName),
  );

  const [tab, setTab] = useState('connectionDetails');

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

  const handleSubmit = (formValues: BigQueryConnectionSchema) => {
    const payload = generateBigQueryRequestPayload({
      driver: 'bigquery',
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
      console.log(err);
      hasuraToast({
        type: 'error',
        title:
          'Error while retrieving database. Please check if the database is of type bigquery.',
      });
    }
  }, [metadataSource, reset]);

  return (
    <div>
      <Heading size="4">
        {isEditMode ? 'Edit BigQuery Connection' : 'Connect BigQuery Database'}
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
                  <Configuration name="configuration" />

                  <div className="my-4">
                    <Collapsible
                      disableContentStyles
                      triggerChildren={
                        <div className="font-semibold text-muted">
                          GraphQL Customization
                        </div>
                      }
                    >
                      <GraphQLCustomization name="customization" />
                    </Collapsible>
                  </div>

                  <Flex justify="end">
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
