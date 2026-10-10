import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { useMetadataHelpers, useMetadataMigration } from '../metadata';
import { SupportedDriver, Table } from '@hasura/shared/types';
import { useErrorNotification } from '../notification';

export type DeleteRemoteRelationshipArgs = {
  name: string;
  source: string;
  table: Table;
};

export const getDeleteRemoteRelationshipType = (driver: SupportedDriver) => {
  const prefix = getDriverPrefix(driver!);
  return `${prefix}_delete_remote_relationship` as const;
};

export const useDeleteRemoteRelationship = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const deleteRemoteRelationship = async (
    args: DeleteRemoteRelationshipArgs,
    onSuccess?: () => void,
  ) => {
    const { source, resource_version } = await fetchSource(args.source);

    const requestBody = {
      type: getDeleteRemoteRelationshipType(source.kind),
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
            message: 'Relationship deleted successfully',
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
    deleteRemoteRelationship,
    ...rest,
  };
};
