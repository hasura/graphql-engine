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
import { getMetadataQuery, MetadataQueryType } from '@/metadata/queryUtils';
import { schema, Schema } from './schema';
import { FormElements } from './FormElements';
import { useDefaultValues } from './hooks';
import { getSchemaKey } from './utils';

type StatusType = {
  title: string;
  message: string;
  type: 'success' | 'error';
};

type MetadataPayloadType = {
  type: allowedMetadataTypes;
  args: { [key: string]: any };
  version?: number;
};

export type RemoteDBRelationshipWidgetProps = {
  /**
   * the data source which the relationship originates from, this is the only mandator prop to render this component
   *
   * @type {DataTarget}
   */
  sourceTableInfo: DataTarget;
  /**
   * Used to render the component in edit mode, passing the relationship name would prefill the form with default values required for the relationship
   *
   * @type {string}
   */
  existingRelationshipName?: string;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete?: (v: StatusType) => void;
};

export const RemoteDBRelationshipWidget = ({
  sourceTableInfo,
  existingRelationshipName,
  onComplete,
}: RemoteDBRelationshipWidgetProps) => {
  // hook to fetch data for existing relationship
  const { data: defaultValues, isLoading, isError } = useDefaultValues({
    sourceTableInfo,
    existingRelationshipName,
  });

  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      const status: StatusType = {
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success',
      };
      fireNotification(status);
      if (onComplete) {
        onComplete(status);
      }
    },
    onError: (error: Error) => {
      const status: StatusType = {
        title: 'Error',
        message: error?.message ?? 'Error while creating the relationship',
        type: 'error',
      };
      fireNotification(status);
      if (onComplete) {
        onComplete(status);
      }
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
      table: {
        [getSchemaKey(sourceTableInfo)]:
          (sourceTableInfo as any).dataset ?? (sourceTableInfo as any).schema,
        name: sourceTableInfo.table,
      },
      name: values.relationshipName,
      definition: {
        to_source: {
          source: values.destination.database,
          table: {
            [getSchemaKey(remote_table as DataTarget)]:
              (remote_table as any).dataset ?? (remote_table as any).schema,
            name: remote_table.table,
          },
          relationship_type: values.relationshipType,
          field_mapping: values.mapping,
        },
      },
    };
    const requestBody = existingRelationshipName
      ? getMetadataQuery(
          'update_remote_relationship' as MetadataQueryType,
          sourceTableInfo.database,
          args
        )
      : getMetadataQuery(
          'create_remote_relationship' as MetadataQueryType,
          sourceTableInfo.database,
          args
        );

    mutation.mutate({
      query: requestBody as MetadataPayloadType,
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
                      value: 'object',
                    },
                    {
                      label: 'Array Relationship',
                      value: 'array',
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
              Save Relationship
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
