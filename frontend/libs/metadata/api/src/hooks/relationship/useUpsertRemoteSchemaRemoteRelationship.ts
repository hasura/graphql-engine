import { RemoteRelationship } from '@hasura/shared/types';
import { useMetadataMigrationMutation } from '../metadata';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';

export type UpsertRemoteSchemaRemoteRelationshipProps = {
  action: 'create' | 'update';
  args: RemoteRelationship & {
    remote_schema: string;
    type_name: string;
  };
};

export function useUpsertRemoteSchemaRemoteRelationship() {
  return useMetadataMigrationMutation(
    async ({ action, args }: UpsertRemoteSchemaRemoteRelationshipProps) => {
      const metadataArgType =
        `${action}_remote_schema_remote_relationship` as const;

      return {
        type: metadataArgType,
        args,
      };
    },
    {
      onSuccess: () => {
        hasuraToast({
          title: 'Success!',
          message: 'Relationship saved successfully',
          type: 'success',
        });
      },
      onError: (error) => {
        showErrorNotification({
          title: 'Error while saving the relationship',
          error,
        });
      },
    },
  );
}
