import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';
import type {
  DataQueryType,
  QualifiedDataSource,
  SelectPermissionDefinition,
  Table,
  TablePermissionDefinition,
} from '@hasura/shared/types';
import { getDropPermissionQuery } from './useDropTablePermission';
import { getCreatePermissionQuery } from './useCreateTablePermission';
import { TMigrationSingleQuery } from '../../api';

type UpdateTablePermissionsArgs = {
  table: Table;
  limitEnabled: boolean;
  role: string;
  permission: TablePermissionDefinition;
  operation: DataQueryType;
  source: QualifiedDataSource;
};

export const useUpdateTablePermissions = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      {
        limitEnabled,
        table,
        role,
        source,
        operation,
        permission,
      }: UpdateTablePermissionsArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      if (operation === 'select' && !limitEnabled) {
        const { limit, ...others } = permission as SelectPermissionDefinition;
        permission = others;
      }

      const upQueries: TMigrationSingleQuery[] = [
        getDropPermissionQuery(operation, source.kind, {
          table,
          source: source.name,
          role,
        }),
        getCreatePermissionQuery(operation, source.kind, {
          table,
          source: source.name,
          role,
          permission,
        }),
      ];
      return mutation.mutate(
        {
          query: { type: 'bulk', args: upQueries },
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Updating permissions failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Permissions updated',
            });

            onSuccess?.();
          },
        },
      );
    },
    [mutation, showErrorNotification],
  );

  return {
    ...mutation,
    mutate,
  };
};
