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
import { Collapsible } from '../../../../new-components/Collapsible';

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
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode ? 'Edit BigQuery Connection' : 'Connect BigQuery Database'}
      </div>
      <Form onSubmit={handleSubmit}>
        <InputField
          name="name"
          label="Database name"
          placeholder="Database name"
        />
        <Configuration name="configuration" />

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
