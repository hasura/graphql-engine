import React from 'react';

import { useConsoleForm } from '../../../../../new-components/Form';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { Button } from '../../../../../new-components/Button';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '../../../../MetadataAPI';

import { useFireNotification } from '../../../../../new-components/Notifications';
import { useFormContext } from 'react-hook-form';
import {
  RelationshipTypeCardRadioGroup,
  RemoteRelOption,
} from './RelationshipTypeCardRadioGroup';

import { FormElements } from './FormElements';
import { generateLhsFields } from '../../utils';
import { rsToRsFormSchema, RsToRsSchema } from '../../types';
import { useDefaultValues } from './hooks';

export type RemoteSchemaToRemoteSchemaFormProps = {
  sourceRemoteSchema: string;
  typeName?: string;
  existingRelationshipName?: string;
  closeHandler: () => void;
  relModeHandler: (v: RemoteRelOption) => void;
  onSuccess?: () => void;
};

type ResetterProps = {
  sourceRemoteSchema: string;
  typeName?: string;
  existingRelationshipName?: string;
};

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

  const { reset } = useFormContext<RsToRsSchema>();

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

// Wrapper to provide Form Context
export const RemoteSchemaToRemoteSchemaForm = (
  props: RemoteSchemaToRemoteSchemaFormProps
) => {
  const {
    sourceRemoteSchema,
    typeName,
    existingRelationshipName,
    closeHandler,
    relModeHandler,
  } = props;
  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success',
      });
      if (closeHandler) closeHandler();
    },
    onError: (error: Error) => {
      fireNotification({
        title: 'Error',
        message: error?.message ?? 'Error while creating the relationship',
        type: 'error',
      });
    },
  });

  const submit = (values: Record<string, unknown>) => {
    const lhs_fields = generateLhsFields(
      values.resultSet as Record<string, unknown>
    );
    const type = existingRelationshipName ? 'update' : 'create';

    const requestBody = {
      type: `${type}_remote_schema_remote_relationship` as allowedMetadataTypes,
      args: {
        remote_schema: values.sourceRemoteSchema,
        type_name: values.rsSourceType,
        name: values.name,
        definition: {
          to_remote_schema: {
            remote_schema: values.referenceRemoteSchema,
            lhs_fields,
            remote_field: values.resultSet,
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
    schema: rsToRsFormSchema,
  });

  return (
    <Form onSubmit={submit} className="p-4">
      <>
        <SetDefaults
          sourceRemoteSchema={sourceRemoteSchema}
          typeName={typeName}
          existingRelationshipName={existingRelationshipName}
        />
        <div className="grid border border-gray-300 rounded shadow-sm p-4 w-full">
          <div className="flex items-center gap-4 w-full mb-md">
            <Button type="button" size="sm" onClick={closeHandler}>
              Cancel
            </Button>
            <p className="font-semibold m-0">
              {existingRelationshipName
                ? 'Edit Relationship'
                : 'Add Relationship'}
            </p>
          </div>

          <hr className="mb-md border-gray-300" />

          {existingRelationshipName ? null : (
            <RelationshipTypeCardRadioGroup
              value="remoteSchema"
              onChange={relModeHandler}
            />
          )}

          <FormElements
            sourceRemoteSchema={sourceRemoteSchema}
            existingRelationshipName={existingRelationshipName}
          />

          {/* submit */}
          <div>
            <Button
              mode="primary"
              size="md"
              type="submit"
              isLoading={mutation.isLoading}
              loadingText={
                existingRelationshipName
                  ? 'Updating relationship'
                  : 'Creating relationship'
              }
              data-test="add-rs-relationship"
            >
              {existingRelationshipName
                ? 'Edit Relationship'
                : 'Add Relationship'}
            </Button>
          </div>

          {!!Object.keys(formState.errors).length && (
            <IndicatorCard status="negative">
              Error saving relationship
            </IndicatorCard>
          )}
        </div>
      </>
    </Form>
  );
};
