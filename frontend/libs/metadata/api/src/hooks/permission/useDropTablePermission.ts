import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import type {
  SupportedDriver,
  DataQueryType,
  Table,
  QualifiedDataSource,
} from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { useErrorNotification } from '../notification';

export const getDropPermissionQuery = (
  action: DataQueryType,
  driver: SupportedDriver,
  args: {
    table: Table;
    role: string;
    source: string;
  },
) => {
  const prefix = getDriverPrefix(driver);
  const queryType = `${prefix}_drop_${action}_permission` as const;

  return {
    type: queryType,
    args,
  };
};

type DropTablePermissionArgs = {
  table: Table;
  source: QualifiedDataSource;
  role: string;
  operation: DataQueryType;
};

export const useDropTablePermission = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const dropTablePermission = (
    { operation, table, role, source }: DropTablePermissionArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const deleteQuery = getDropPermissionQuery(operation, source.kind, {
      table,
      source: source.name,
      role,
    });

    return mutate(
      {
        query: deleteQuery,
      },
      {
        onError: (error, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Dropping permissions failed',
            error,
          });
          options?.onError?.(error, variables, onMutateResult, context);
        },
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Permissions dropped',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    ...rest,
    dropTablePermission,
  };
};
