import React from 'react';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { fireNotification } from '@/new-components/Notifications';
import { DataTarget } from '@/features/Datasources';
import { InputField, Form } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { getMetadataQuery, MetadataQueryType } from '@/metadata/queryUtils';
import { FormElementDbToRs } from './FormElementDbToRs';
import { DbToRsSchema, schema } from './schema';
import { refRemoteSchemaSelectorKey } from '../../RemoteSchemaRelationships/components/RefRsSelector';
import { generateLhsFields } from '../../RemoteSchemaRelationships/utils';
import { RemoteRelationship } from '../../RemoteSchemaRelationships/types';

export type DbToRsFormProps = {
  /**
   * the data source which the relationship originates from, this is the only mandator prop to render this component
   *
   * @type {DataTarget}
   */
  sourceTableInfo: DataTarget;
  /**
   * Used to render the component in edit mode, passing the relationship object would expand the tree component and prefill the values from the existing relationship, also this would disbale the relationship name field
   *
   * @type {RemoteRelationship}
   */
  selectedRelationship?: RemoteRelationship;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete?: (v: {
    title: string;
    message: string;
    type: 'success' | 'error';
  }) => void;
};

type MetadataPayloadType = {
  type: allowedMetadataTypes;
  args: { [key: string]: any };
  version?: number;
};

export const DbToRsForm = ({
  sourceTableInfo,
  selectedRelationship,
  onComplete,
}: DbToRsFormProps) => {
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
      const status = {
        title: 'Error',
        message: error?.message ?? 'Error while creating the relationship',
        type: 'error' as 'success' | 'error',
      };
      fireNotification(status);
      if (onComplete) onComplete(status);
    },
  });

  const defaultValues = {
    resultSet:
      selectedRelationship?.definition?.to_remote_schema.remote_field ?? {},
    relationshipName: selectedRelationship?.name ?? '',
    [refRemoteSchemaSelectorKey]:
      selectedRelationship?.definition?.to_remote_schema?.remote_schema ?? '',
    relationship: {},
  };

  const submit = (values: DbToRsSchema) => {
    const lhs_fields = generateLhsFields(
      values.resultSet as Record<string, unknown>
    );

    const table = {
      name: sourceTableInfo.table,
      ...('schema' in sourceTableInfo && { schema: sourceTableInfo.schema }),
      ...('dataset' in sourceTableInfo && { dataset: sourceTableInfo.dataset }),
    };

    const args = {
      name: values.relationshipName,
      table,
      source: sourceTableInfo.database,
      definition: {
        to_remote_schema: {
          remote_schema: values.referenceRemoteSchema,
          lhs_fields,
          remote_field: values.resultSet,
        },
      },
    };
    const requestBody = getMetadataQuery(
      'create_remote_relationship' as MetadataQueryType,
      sourceTableInfo.database,
      args
    );

    mutation.mutate({
      source: sourceTableInfo.database,
      query: requestBody as MetadataPayloadType,
      migrationName: 'createDBToRSRelationship',
    });
  };

  return (
    <Form schema={schema} onSubmit={submit} options={{ defaultValues }}>
      {() => (
        <div>
          <div className="w-full sm:w-6/12 mb-md">
            <div className="mb-md">
              <InputField
                name="relationshipName"
                label="Name"
                placeholder="Relationship name"
                dataTest="local-db-to-db-rel-name"
                disabled={!!selectedRelationship}
              />
            </div>
          </div>
          <FormElementDbToRs
            sourceTableInfo={sourceTableInfo}
            selectedRelationship={selectedRelationship}
          />

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
      )}
    </Form>
  );
};
