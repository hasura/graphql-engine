import { useCallback } from 'react';
import { useMetadataMigration } from '../metadata/useMetadataMigration';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';
import type {
  RemoteSchema,
  RemoteSchemaPermission,
} from '@hasura/shared/types';
import { getDropRemoteSchemaPermissionQuery } from './useDropRemoteSchemaPermissions';
import { TMigrationSingleQuery } from '../../api';

const getCreateRemoteSchemaPermissionQuery = (
  role: string,
  remoteSchemaName: string,
  schemaDefinition: string,
) => {
  return {
    type: 'add_remote_schema_permissions' as const,
    args: {
      remote_schema: remoteSchemaName,
      role: role,
      definition: {
        schema: schemaDefinition,
      },
    },
  };
};

const getSaveRemoteSchemaPermissionQueries = (
  role: string,
  newRole: string,
  allPermissions: RemoteSchemaPermission[] | undefined,
  remoteSchemaName: string,
  schemaDefinition: string,
) => {
  const permRole = newRole || role;
  const existingPerm = allPermissions?.find((p) => p.role === permRole);
  const queries: Record<string, any>[] = [];
  if (newRole || (!newRole && !existingPerm)) {
    queries.push(
      getCreateRemoteSchemaPermissionQuery(
        permRole,
        remoteSchemaName,
        schemaDefinition,
      ),
    );
  }

  if (existingPerm) {
    queries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName),
      getCreateRemoteSchemaPermissionQuery(
        permRole,
        remoteSchemaName,
        schemaDefinition,
      ),
    );
  }

  return {
    type: 'bulk' as const,
    args: queries as TMigrationSingleQuery[],
  };
};

type SaveRemoteSchemaPermissionArgs = {
  currentRemoteSchema: RemoteSchema;
  role: string;
  newRole: string;
  schemaDefinition: string;
};

export const useSaveRemoteSchemaPermission = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const mutate = useCallback(
    async (
      args: SaveRemoteSchemaPermissionArgs,
      onSuccess?: () => void,
      onError?: (err: unknown) => void,
    ) => {
      const query = getSaveRemoteSchemaPermissionQueries(
        args.role,
        args.newRole,
        args.currentRemoteSchema.permissions,
        args.currentRemoteSchema.name,
        args.schemaDefinition,
      );
      return mutation.mutate(
        {
          query,
        },
        {
          onError: (error) => {
            showErrorNotification({
              title: 'Saving permission failed',
              error,
            });
            onError?.(error);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Permission saved successfully',
            });

            if (onSuccess) onSuccess();
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
