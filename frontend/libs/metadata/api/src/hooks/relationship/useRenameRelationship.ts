import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { Table } from '@hasura/shared/types';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import { useErrorNotification } from '../notification';

export type RenameRelationshipArgs = {
  name: string;
  source: string;
  table: Table;
  new_name: string;
};

type MutationOptions = MetadataMigrationOptions;

export const useRenameRelationship = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const renameRelationship = async (
    args: RenameRelationshipArgs,
    options?: MutationOptions,
  ) => {
    const { source: currentSource, resource_version } = await fetchSource(
      args.source,
    );
    const prefix = getDriverPrefix(currentSource.kind!);
    const requestBody = {
      type: `${prefix}_rename_relationship` as const,
      args,
      resource_version,
    };

    return mutate(
      {
        query: requestBody,
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Success!',
            message: 'Relationship renamed successfully',
            type: 'success',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (error, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error',
            error,
          });
          options?.onError?.(error, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    renameRelationship,
    ...rest,
  };
};
