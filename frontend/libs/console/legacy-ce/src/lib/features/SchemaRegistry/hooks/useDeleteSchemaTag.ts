import * as React from 'react';
import { useMutation /*useQueryClient*/ } from 'react-query';
import { DELETE_SCHEMA_TAG } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { DeleteSchemaRegistryTagResponseWithError } from '../types';
import { hasuraToast } from '../../../new-components/Toasts';

type DeleteSchemaRegistryTagMutationFnArgs = {
  ID: string;
};

export const useDeleteSchemaTag = (onDelete: (id: string) => void) => {
  const deleteSchemaRegistryTagMutationFn = (variables: { ID: string }) => {
    return schemaRegsitryControlPlaneClient.query<
      DeleteSchemaRegistryTagResponseWithError,
      { ID: string }
    >(DELETE_SCHEMA_TAG, variables);
  };

  const deleteSchemaRegistryTagMutation = useMutation(
    (args: DeleteSchemaRegistryTagMutationFnArgs) =>
      deleteSchemaRegistryTagMutationFn(args),
    {
      onSuccess: data => {
        if (data.data)
          onDelete(data.data?.delete_schema_registry_tags_by_pk.id);
      },
      onError: () => {
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message:
            'Something went wrong while deleting the tag for Schema Registry',
        });
      },
    }
  );

  return {
    deleteSchemaRegistryTagMutation,
  };
};
