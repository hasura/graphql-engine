import React from 'react';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '../../../MetadataAPI';
import { useFireNotification } from '../../../../new-components/Notifications';
import { DataTarget } from '../../../Datasources';
import { InputField, SimpleForm } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import {
  getMetadataQuery,
  MetadataQueryType,
} from '../../../../metadata/queryUtils';
import { FormElementDbToRs } from './FormElementDbToRs';
import { schema } from './schema';
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

export const DbToRsForm = ({
  sourceTableInfo,
  selectedRelationship,
  onComplete,
}: DbToRsFormProps) => {
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
      const status = {
        title: 'Error',
        message: error?.message ?? 'Error while creating the relationship',
        type: 'error' as 'success' | 'error',
      };
      fireNotification(status);
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

  const submit = (values: Record<string, unknown>) => {
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
      query: requestBody as MetadataPayloadType,
    });
  };

  return (
    <SimpleForm
      schema={schema}
      onSubmit={submit}
      options={{ defaultValues }}
      className="p-4"
    >
      <div>
        <div className="w-full sm:w-6/12 mb-md">
          <div className="mb-md">
            <InputField
              id="relationshipName"
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
    </SimpleForm>
  );
};
