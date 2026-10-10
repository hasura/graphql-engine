import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import {
  SourceToRemoteSchemaRelationship,
  SourceToSourceRelationship,
  Table,
} from '@hasura/shared/types';
import { useMetadataHelpers, useMetadataMigration } from '../metadata';
import { useErrorNotification } from '../notification';

export type CreateRemoteRelationshipArgs = {
  name: string;
  source: string;
  table: Table;
  definition:
    | SourceToRemoteSchemaRelationship['definition']
    | SourceToSourceRelationship['definition'];
};

export const useCreateRemoteRelationship = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const createRemoteRelationship = async (
    args: CreateRemoteRelationshipArgs,
    onSuccess?: () => void,
  ) => {
    const { source: currentSource, resource_version } = await fetchSource(
      args.source,
    );
    const prefix = getDriverPrefix(currentSource.kind!);
    const requestBody = {
      type: `${prefix}_create_remote_relationship` as const,
      args,
      resource_version,
    };

    return mutate(
      {
        query: requestBody,
      },
      {
        onSuccess: () => {
          hasuraToast({
            title: 'Success!',
            message: 'Relationship saved successfully',
            type: 'success',
          });

          onSuccess?.();
        },
        onError: (error: unknown) => {
          showErrorNotification({
            title: 'Error',
            error,
          });
        },
      },
    );
  };

  return {
    createRemoteRelationship,
    ...rest,
  };
};
