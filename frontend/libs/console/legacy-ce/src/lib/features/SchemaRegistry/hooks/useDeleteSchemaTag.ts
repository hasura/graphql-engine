import { useMutation } from '@tanstack/react-query';
import { DELETE_SCHEMA_TAG } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { DeleteSchemaRegistryTagResponseWithError } from '../types';
import { showErrorNotification } from '@hasura/shared/ui';

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

  const deleteSchemaRegistryTagMutation = useMutation({
    mutationFn: (args: DeleteSchemaRegistryTagMutationFnArgs) =>
      deleteSchemaRegistryTagMutationFn(args),
    onSuccess: (data) => {
      if (data.data) onDelete(data.data?.delete_schema_registry_tags_by_pk.id);
    },
    onError: (error) => {
      showErrorNotification({
        error,
        title: 'Deleting the tag for Schema Registry failed',
      });
    },
  });

  return {
    deleteSchemaRegistryTagMutation,
  };
};
