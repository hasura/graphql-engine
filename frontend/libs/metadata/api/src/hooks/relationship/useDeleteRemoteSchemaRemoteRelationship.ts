import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from '../metadata';

export type DeleteRemoteSchemaRemoteRelationshipProps = {
  remote_schema: string;
  type_name: string;
  name: string;
};

export function useDeleteRemoteSchemaRemoteRelationship() {
  return useMetadataMigrationMutation(
    async (args: DeleteRemoteSchemaRemoteRelationshipProps) => {
      return {
        type: 'delete_remote_schema_remote_relationship',
        args,
      };
    },
    {
      onSuccess: () => {
        hasuraToast({
          title: 'Success!',
          message: 'Relationship deleted successfully',
          type: 'success',
        });
      },
      onError: (error: Error) => {
        showErrorNotification({
          title: 'Error while deleting the relationship',
          error: error?.message,
        });
      },
    },
  );
}
