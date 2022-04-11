import React from 'react';
import { Form } from '@/new-components/Form';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { fireNotification } from '@/new-components/Notifications';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Button } from '@/new-components/Button';
import { FormElements } from './FormElements';
import { schema, Schema } from './schema';
import {
  RelationshipTypeCardRadioGroup,
  RemoteRelOption,
} from '../RemoteSchemaToRemoteSchemaForm/RelationshipTypeCardRadioGroup';

export type RemoteSchemaToDbFormProps = {
  sourceRemoteSchema: string;
  closeHandler?: () => void;
  onSuccess?: () => void;
  relModeHandler: (v: RemoteRelOption) => void;
};

export const RemoteSchemaToDbForm = ({
  sourceRemoteSchema,
  closeHandler,
  onSuccess,
  relModeHandler,
}: RemoteSchemaToDbFormProps) => {
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success',
      });
      if (onSuccess) onSuccess();
    },
    onError: () => {
      fireNotification({
        title: 'Error',
        message: 'Error while creating the relationship',
        type: 'error',
      });
    },
  });

  const defaultValues: Schema = {
    relationshipType: 'array',
    relationshipName: '',
    mapping: [],
    database: '',
    schema: '',
    table: '',
    typeName: '',
    driver: '',
    sourceRemoteSchema,
  };

  const submit = (values: Schema) => {
    const field_mapping: Record<string, string> = values.mapping.reduce(
      (acc, new_value) => {
        acc[new_value.field] = new_value.column;
        return acc;
      },
      {} as Record<string, string>
    );

    const requestBody = {
      type: 'create_remote_schema_remote_relationship' as allowedMetadataTypes,
      args: {
        remote_schema: sourceRemoteSchema,
        type_name: values.typeName,
        name: values.relationshipName,
        definition: {
          to_source: {
            source: values.database,
            table:
              values.driver === 'bigquery'
                ? { dataset: values.schema, name: values.table }
                : { schema: values.schema, name: values.table },
            relationship_type: values.relationshipType,
            field_mapping,
          },
        },
      },
    };
    mutation.mutate({
      source: '',
      query: requestBody,
      migrationName: 'createRSToDBRelationship',
    });
  };

  return (
    <Form schema={schema} options={{ defaultValues }} onSubmit={submit}>
      {options => (
        <>
          <div className="grid border border-gray-300 rounded shadow-sm p-4">
            <div className="flex items-center mb-md">
              <Button type="button" size="sm" onClick={closeHandler}>
                Cancel
              </Button>
              <span className="font-semibold text-muted ml-sm">
                Create New Relationship
              </span>
            </div>
            <hr className="mb-md border-gray-300" />

            {/* relationship meta */}
            <RelationshipTypeCardRadioGroup
              value="remoteDB"
              onChange={relModeHandler}
            />

            <FormElements sourceRemoteSchema={sourceRemoteSchema} />
            {/* submit */}
            <div>
              <Button
                iconPosition="start"
                mode="primary"
                size="sm"
                type="submit"
                isLoading={mutation.isLoading}
                loadingText="Creating relationship"
                data-test="add-rs-relationship"
              >
                Add Relationship
              </Button>
            </div>
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
