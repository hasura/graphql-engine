import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { useMetadataHelpers, useMetadataMigration } from '../metadata';
import { CreateRemoteRelationshipArgs } from './useCreateRemoteRelationship';
import { useErrorNotification } from '../notification';

type UpdateRemoteRelationshipArgs = CreateRemoteRelationshipArgs;

export const useUpdateRemoteRelationship = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const updateRemoteRelationship = async (
    args: UpdateRemoteRelationshipArgs,
    onSuccess?: () => void,
  ) => {
    const { source: currentSource, resource_version } = await fetchSource(
      args.source,
    );

    const prefix = getDriverPrefix(currentSource.kind!);
    const requestBody = {
      type: `${prefix}_update_remote_relationship` as const,
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
    updateRemoteRelationship,
    ...rest,
  };
};
