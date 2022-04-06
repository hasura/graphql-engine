import React from 'react';

import { Form } from '@/new-components/Form';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Button } from '@/new-components/Button';

import { fireNotification } from '@/new-components/Notifications';
import {
  RemoteRelOption,
  RemoteRelRadioCardPicker,
} from '@/new-components/RemoteSchemaAndDbRadioCardPicker';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';

import { FormElements } from './FormElements';
import { generateLhsFields } from '../../utils';
import { rsToRsFormSchema, RsToRsSchema } from '../../types';

export type RemoteSchemaToRemoteSchemaFormProps = {
  sourceRemoteSchema: string;
  closeHandler: () => void;
  relModeHandler: (v: RemoteRelOption) => void;
};

// Wrapper to provide Form Context
export const RemoteSchemaToRemoteSchemaForm = (
  props: RemoteSchemaToRemoteSchemaFormProps
) => {
  const { sourceRemoteSchema, closeHandler, relModeHandler } = props;
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship saved successfully',
        type: 'success',
      });
      if (closeHandler) closeHandler();
    },
    onError: () => {
      fireNotification({
        title: 'Error',
        message: 'Error while creating the relationship',
        type: 'error',
      });
    },
  });
  const defaultValues: RsToRsSchema = React.useMemo(
    () => ({
      relationshipMethod: 'remoteSchema',
      name: '',
      sourceRemoteSchema,
      rsSourceType: '',
      referenceRemoteSchema: '',
      resultSet: '{}',
    }),
    [sourceRemoteSchema]
  );
  const submit = (values: RsToRsSchema) => {
    const lhs_fields = generateLhsFields(
      values.resultSet as Record<string, unknown>
    );
    // testing payload
    const requestBody = {
      type: 'create_remote_schema_remote_relationship' as allowedMetadataTypes,
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
      source: '',
      query: requestBody,
      migrationName: 'createRSToRSRelationship',
    });
  };

  return (
    <Form
      schema={rsToRsFormSchema}
      options={{ defaultValues }}
      onSubmit={submit}
    >
      {options => (
        <div className="grid gap-4 border border-gray-300 rounded shadow-sm p-4 w-full">
          <div className="flex items-center gap-4 w-full">
            <Button type="button" size="sm" onClick={closeHandler}>
              Cancel
            </Button>
            <p className="font-semibold m-0">Create New Relationship</p>
          </div>

          <hr />

          <RemoteRelRadioCardPicker
            value="remoteSchema"
            onChange={relModeHandler}
          />

          <FormElements sourceRemoteSchema={sourceRemoteSchema} />

          {/* submit */}
          <div>
            <Button
              mode="primary"
              size="sm"
              type="submit"
              isLoading={mutation.isLoading}
              loadingText="Creating relationship"
            >
              Add Relationship
            </Button>
          </div>

          {!!Object.keys(options.formState.errors).length && (
            <IndicatorCard status="negative">
              Error saving relationship
            </IndicatorCard>
          )}
        </div>
      )}
    </Form>
  );
};
