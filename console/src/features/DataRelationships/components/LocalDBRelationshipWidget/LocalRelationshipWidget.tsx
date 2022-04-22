import React from 'react';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { fireNotification } from '@/new-components/Notifications';
import { DataTarget } from '@/features/Datasources';
import { InputField, Select, Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { getMetadataQuery, MetadataQueryType } from '@/metadata/queryUtils';
import { schema, Schema } from './schema';
import { FormElements } from './FormElements';
import { useDefaultValues } from './hooks';

export type LocalRelationshipWidgetProps = {
  sourceTableInfo: DataTarget;
  existingRelationshipName?: string;
};

type MetadataPayloadType = {
  type: allowedMetadataTypes;
  args: { [key: string]: any };
  version?: number;
};

export const LocalRelationshipWidget = ({
  sourceTableInfo,
  existingRelationshipName,
}: LocalRelationshipWidgetProps) => {
  // hook to fetch data for existing relationship
  const { data: defaultValues, isLoading, isError } = useDefaultValues({
    sourceTableInfo,
    existingRelationshipName,
  });

  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success',
      });
    },
    onError: () => {
      fireNotification({
        title: 'Error',
        message: 'Error while creating the relationship',
        type: 'error',
      });
    },
  });

  const submit = (values: Schema) => {
    const remote_table: {
      database?: string;
      schema?: string;
      dataset?: string;
      table: string;
    } = { ...values.destination };
    delete remote_table.database;

    const args = {
      source: sourceTableInfo.database,
      table: sourceTableInfo.table,
      name: values.relationshipName,
      using: {
        manual_configuration: {
          remote_table,
          mapping: values.mapping,
        },
      },
    };
    const requestBody = getMetadataQuery(
      values.relationshipType as MetadataQueryType,
      sourceTableInfo.database,
      args
    );

    mutation.mutate({
      source: '',
      query: requestBody as MetadataPayloadType,
      migrationName: 'createLocalDBToDBRelationship',
    });
  };

  if (isLoading) {
    return <div>Loading relationship data...</div>;
  }

  if (isError) {
    return <div>Something went wrong while loading relationship data</div>;
  }

  return (
    <Form schema={schema} onSubmit={submit} options={{ defaultValues }}>
      {options => (
        <>
          <div>
            <div className="w-full sm:w-6/12 mb-md">
              <div className="mb-md">
                <InputField
                  name="relationshipName"
                  label="Name"
                  placeholder="Relationship name"
                  dataTest="local-db-to-db-rel-name"
                />
              </div>

              <div className="mb-md">
                <Select
                  name="relationshipType"
                  label="Type"
                  dataTest="local-db-to-db-select-rel-type"
                  placeholder="Select a relationship type..."
                  options={[
                    {
                      label: 'Object Relationship',
                      value: 'create_object_relationship',
                    },
                    {
                      label: 'Array Relationship',
                      value: 'create_array_relationship',
                    },
                  ]}
                />
              </div>
            </div>
            <FormElements />

            <Button
              mode="primary"
              type="submit"
              isLoading={mutation.isLoading}
              loadingText="Saving relationship"
              data-test="add-local-db-relationship"
            >
              Add Relationship
            </Button>
          </div>

          {!!Object.keys(options.formState.errors).length && (
            <IndicatorCard status="negative">
              Error saving relationship
            </IndicatorCard>
          )}
        </>
      )}
    </Form>
  );
};
