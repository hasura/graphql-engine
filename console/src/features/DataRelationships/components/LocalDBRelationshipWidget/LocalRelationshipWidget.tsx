import React from 'react';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { DataTarget } from '@/features/Datasources';
import { InputField, Select, Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import {
  DataSourceDriver,
  getDataSourcePrefix,
  getMetadataQuery,
  MetadataQueryType,
} from '@/metadata/queryUtils';
import { Driver } from '@/dataSources';
import { schema, Schema } from './schema';
import { FormElements } from './FormElements';
import { useDefaultValues } from './hooks';
import { getSchemaKey } from '../RemoteDBRelationshipWidget/utils';

export type LocalRelationshipWidgetProps = {
  sourceTableInfo: DataTarget;
  existingRelationshipName?: string;
  driver: Driver;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete?: (v: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
};

type MetadataPayloadType = {
  type: allowedMetadataTypes;
  args: { [key: string]: any };
  version?: number;
};

export const LocalRelationshipWidget = ({
  sourceTableInfo,
  existingRelationshipName,
  onComplete,
}: LocalRelationshipWidgetProps) => {
  // hook to fetch data for existing relationship
  const { data: defaultValues, isLoading, isError } = useDefaultValues({
    sourceTableInfo,
    existingRelationshipName,
  });
  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      const status = {
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success' as 'success' | 'error',
      };
      fireNotification(status);
      if (onComplete) onComplete(status);
    },
    onError: (error: Error) => {
      fireNotification({
        title: 'Error',
        message: error?.message ?? 'Error while creating the relationship',
        type: 'error',
      });
    },
  });

  const renameRelationship = (values: Schema) => {
    const sourcePrefix = getDataSourcePrefix(
      sourceTableInfo.database as DataSourceDriver
    );

    const payload = {
      type: `${sourcePrefix}rename_relationship`,
      args: {
        table: sourceTableInfo.table,
        name: existingRelationshipName,
        source: sourceTableInfo.database,
        new_name: values.relationshipName,
      },
    };

    mutation.mutate({
      query: payload as MetadataPayloadType,
    });
  };

  const createRelationship = (values: Schema) => {
    const remote_table: {
      database?: string;
      schema?: string;
      dataset?: string;
      table: string;
    } = { ...values.destination };
    delete remote_table.database;

    const args = {
      source: sourceTableInfo.database,
      table: {
        [getSchemaKey(sourceTableInfo)]:
          (sourceTableInfo as any).dataset ?? (sourceTableInfo as any).schema,
        name: sourceTableInfo.table,
      },
      name: values.relationshipName,
      using: {
        manual_configuration: {
          remote_table: {
            [getSchemaKey(remote_table as DataTarget)]:
              (remote_table as any).dataset ?? (remote_table as any).schema,
            name: remote_table.table,
          },
          column_mapping: values.mapping,
        },
      },
    };
    const requestBody = getMetadataQuery(
      values.relationshipType as MetadataQueryType,
      sourceTableInfo.database,
      args
    );

    mutation.mutate({
      query: requestBody as MetadataPayloadType,
    });
  };
  const submit = (values: Schema) => {
    if (existingRelationshipName) {
      return renameRelationship(values);
    }
    return createRelationship(values);
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
              {!existingRelationshipName && (
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
              )}
            </div>
            {!existingRelationshipName && <FormElements />}

            <Button
              mode="primary"
              type="submit"
              isLoading={mutation.isLoading}
              loadingText="Saving relationship"
              data-test="add-local-db-relationship"
            >
              {existingRelationshipName ? 'Rename ' : 'Save '} Relationship
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
