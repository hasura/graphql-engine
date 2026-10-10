import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '../../metadata/useMetadataMigration';
import { useMetadataHelpers } from '../../metadata';
import { DataQueryType, TablePermission } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../../notification';
import { ClonePermissionSchema } from './schema';
import { getClonePermissionsArgs } from './utils';

export * from './schema';

export interface ClonePermissionsArgs {
  dataSourceName: string;
  table: unknown;
  queryType: DataQueryType;
  to: ClonePermissionSchema;
  from: TablePermission;
}

export const useClonePermissions = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const clonePermissions = async (
    { dataSourceName, ...rest }: ClonePermissionsArgs,
    mutationOptions?: MetadataMigrationOptions,
  ) => {
    const { source, resource_version } = await fetchSource(dataSourceName);

    return mutate(
      {
        query: {
          type: 'bulk',
          args: getClonePermissionsArgs({
            source,
            ...rest,
          }),
          resource_version,
        },
      },
      {
        ...mutationOptions,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Permissions cloned successfully!',
          });

          mutationOptions?.onSuccess?.(
            data,
            variables,
            onMutateResult,
            context,
          );
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Cloning permissions failed',
            error: err,
          });
          mutationOptions?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return { clonePermissions, ...rest };
};
