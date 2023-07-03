import * as React from 'react';
import { useMutation } from 'react-query';
import { ADD_SCHEMA_TAG } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { CreateSchemaRegistryTagResponseWithError } from '../types';
import { hasuraToast } from '../../../new-components/Toasts';
import { SchemaRegistryTag } from '../types';

type createSchemaRegistryTagMutationFnArgs = {
  tagName: string;
  projectId: string;
  entryHash: string;
  color: string;
};

export const useCreateSchemaTag = (
  onSuccess: (data: SchemaRegistryTag) => void
) => {
  const createSchemaRegistryTagMutationFn = (variables: {
    tagName: string;
    projectId: string;
    entryHash: string;
    color: string;
  }) => {
    return schemaRegsitryControlPlaneClient.query<
      CreateSchemaRegistryTagResponseWithError,
      { tagName: string; projectId: string; entryHash: string; color: string }
    >(ADD_SCHEMA_TAG, variables);
  };

  const createSchemaRegistryTagMutation = useMutation(
    (args: createSchemaRegistryTagMutationFnArgs) =>
      createSchemaRegistryTagMutationFn(args),
    {
      onSuccess: data => {
        if (data.errors && data.errors.length > 0) {
          if (
            data.errors[0].message ||
            data.errors[0].message ===
              'Uniqueness violation. duplicate key value violates unique constraint "schema_registry_tags_entry_hash_name_key"'
          ) {
            hasuraToast({
              type: 'error',
              title: 'Error!',
              message: 'Tag already exists!',
            });
          } else {
            hasuraToast({
              type: 'error',
              title: 'Error!',
              message: 'Something unexpected happened!',
            });
          }
        } else {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Tag created successfully!',
          });

          if (data.data) onSuccess(data.data.insert_schema_registry_tags_one);
        }
      },
      onError: () => {
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message:
            'Something went wrong while creating the tag for Schema Registry',
        });
      },
    }
  );

  return {
    createSchemaRegistryTagMutation,
  };
};
