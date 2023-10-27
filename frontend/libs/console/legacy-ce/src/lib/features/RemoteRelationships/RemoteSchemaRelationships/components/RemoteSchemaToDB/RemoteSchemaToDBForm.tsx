import React from 'react';
import { useFormContext } from 'react-hook-form';
import { useConsoleForm } from '../../../../../new-components/Form';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '../../../../MetadataAPI';
import { useFireNotification } from '../../../../../new-components/Notifications';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { Button } from '../../../../../new-components/Button';
import { FormElements } from './FormElements';
import { schema, Schema } from './schema';
import { useDefaultValues } from './hooks';
import {
  RelationshipTypeCardRadioGroup,
  RemoteRelOption,
} from '../RemoteSchemaToRemoteSchemaForm/RelationshipTypeCardRadioGroup';

export type RemoteSchemaToDbFormProps = {
  sourceRemoteSchema: string;
  typeName?: string;
  existingRelationshipName?: string;
  closeHandler?: () => void;
  onSuccess?: () => void;
  relModeHandler: (v: RemoteRelOption) => void;
};

type ResetterProps = {
  sourceRemoteSchema: string;
  typeName?: string;
  existingRelationshipName?: string;
};

// this needs to be a component because it needs to have access to form context
// this allows default values to be updated if form inputs change
const SetDefaults = ({
  sourceRemoteSchema,
  typeName,
  existingRelationshipName,
}: ResetterProps) => {
  const {
    data: defaultValues,
    isLoading,
    isError,
  } = useDefaultValues({
    sourceRemoteSchema,
    typeName,
    remoteRelationshipName: existingRelationshipName,
  });

  const { reset } = useFormContext<Schema>();

  React.useEffect(() => {
    reset(defaultValues);
  }, [defaultValues, reset]);

  if (isError) {
    return <div>Error loading schema details</div>;
  }

  if (isLoading && existingRelationshipName) {
    return <div>Loading existing schema...</div>;
  }

  return null;
};

export const RemoteSchemaToDbForm = ({
  sourceRemoteSchema,
  typeName,
  existingRelationshipName,
  closeHandler,
  onSuccess,
  relModeHandler,
}: RemoteSchemaToDbFormProps) => {
  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success',
      });
      if (onSuccess) onSuccess();
    },
    onError: (error: Error) => {
      fireNotification({
        title: 'Error',
        message: error?.message ?? 'Error while creating the relationship',
        type: 'error',
      });
    },
  });

  const submit = (values: Schema) => {
    const field_mapping: Record<string, string> = values.mapping.reduce(
      (acc, new_value) => {
        acc[new_value.field] = new_value.column;
        return acc;
      },
      {} as Record<string, string>
    );

    const metadataArgType = existingRelationshipName
      ? ('update_remote_schema_remote_relationship' as allowedMetadataTypes)
      : ('create_remote_schema_remote_relationship' as allowedMetadataTypes);

    const requestBody = {
      type: metadataArgType,
      args: {
        remote_schema: sourceRemoteSchema,
        type_name: values.typeName,
        name: values.relationshipName,
        definition: {
          to_source: {
            source: values?.target?.dataSourceName,
            table: values?.target?.table,
            relationship_type: values.relationshipType,
            field_mapping,
          },
        },
      },
    };
    mutation.mutate({
      query: requestBody,
    });
  };

  const {
    methods: { formState },
    Form,
  } = useConsoleForm({
    schema,
  });

  return (
    <Form onSubmit={submit} className="p-4">
      <>
        <SetDefaults
          sourceRemoteSchema={sourceRemoteSchema}
          typeName={typeName}
          existingRelationshipName={existingRelationshipName}
        />
        <div className="grid border border-gray-300 rounded shadow-sm p-4">
          <div className="flex items-center mb-md">
            <Button type="button" size="sm" onClick={closeHandler}>
              Cancel
            </Button>
            <span className="font-semibold ml-sm">
              {existingRelationshipName
                ? 'Edit Relationship'
                : 'Add Relationship'}
            </span>
          </div>
          <hr className="mb-md border-gray-300" />

          {/* relationship meta */}
          {existingRelationshipName ? null : (
            <RelationshipTypeCardRadioGroup
              value="remoteDB"
              onChange={relModeHandler}
            />
          )}

          <FormElements
            sourceRemoteSchema={sourceRemoteSchema}
            existingRelationshipName={existingRelationshipName ?? ''}
          />
          {/* submit */}
          <div>
            <Button
              iconPosition="start"
              mode="primary"
              size="md"
              type="submit"
              isLoading={mutation.isLoading}
              loadingText="Saving relationship"
              data-test="add-rs-relationship"
            >
              {existingRelationshipName
                ? 'Edit Relationship'
                : 'Add Relationship'}
            </Button>
          </div>
        </div>

        {!!Object.keys(formState.errors).length && (
          <IndicatorCard status="negative">
            Error saving relationship
          </IndicatorCard>
        )}
      </>
    </Form>
  );
};
