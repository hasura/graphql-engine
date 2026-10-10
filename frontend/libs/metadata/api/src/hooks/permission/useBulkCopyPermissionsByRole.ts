import { useCallback } from 'react';
import { hasuraToast } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';
import { DataQueryType, permissionToKey, Table } from '@hasura/shared/types';
import { areTablesEqual } from '@hasura/metadata/helpers';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import { getCreatePermissionQuery } from './useCreateTablePermission';
import { TMigrationSingleQuery } from '../../api';
import { getBulkDropPermissionArgs } from './useBulkDeletePermissions';

type CopySchemaPermissionsByRoleArgs = {
  source: string;
  fromRole: string;
  toRoles: string[];
  actions: DataQueryType[];
} & (
  | {
      allTables: false;
      /** Restrict the copy to a single table. Ignored when not provided (applies to the whole schema/source). */
      fromTable: Table;
      toTable: Table;
    }
  | {
      allTables: true;
    }
);

const errorTitle = 'Copying permissions failed!';

export const useBulkCopyPermissionsByRole = () => {
  const { mutate, ...mutation } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();

  const bulkCopyPermissionsByRole = useCallback(
    async (
      {
        fromRole,
        toRoles,
        actions,
        source,
        ...rest
      }: CopySchemaPermissionsByRoleArgs,
      options?: MetadataMigrationOptions,
    ) => {
      const { source: currentSource, resource_version } =
        await fetchSource(source);

      const args: TMigrationSingleQuery[] = [];
      const permissionKeys = actions.map((action) => permissionToKey[action]);

      if (rest.allTables) {
        args.push(
          ...getBulkDropPermissionArgs({
            roles: toRoles,
            source: currentSource,
            tables: currentSource.tables,
            permissionKeys,
          }),
        );
        args.push(
          ...toRoles.flatMap((role) =>
            actions.flatMap((action) =>
              currentSource.tables.flatMap((t) => {
                return (
                  t[permissionToKey[action]]?.map((p) =>
                    getCreatePermissionQuery(action, currentSource.kind, {
                      table: t.table,
                      source,
                      role,
                      permission: p.permission,
                    }),
                  ) ?? []
                );
              }),
            ),
          ),
        );
      } else {
        const fromTable = currentSource.tables.find((t) =>
          areTablesEqual(t.table, rest.fromTable),
        );
        if (fromTable) {
          let toTables = currentSource.tables;
          if (rest.toTable) {
            const toTable = currentSource.tables.find((t) =>
              areTablesEqual(t.table, rest.toTable),
            );
            toTables = toTable ? [toTable] : [];
          }

          if (toTables.length) {
            args.push(
              ...getBulkDropPermissionArgs({
                roles: toRoles,
                source: currentSource,
                tables: toTables,
                permissionKeys,
              }),
            );

            args.push(
              ...toRoles.flatMap((role) =>
                actions.flatMap((action) =>
                  toTables.flatMap((table) => {
                    // if permissions are being applied to a different table
                    // columns and presets should be blank
                    const clearColumnsAndPresets = !areTablesEqual(
                      fromTable.table,
                      table.table,
                    );

                    return (
                      fromTable[permissionToKey[action]]
                        ?.filter((p) => p.role === fromRole)
                        .map((p) =>
                          getCreatePermissionQuery(action, currentSource.kind, {
                            table: table.table,
                            source,
                            role,
                            permission: clearColumnsAndPresets
                              ? {
                                  ...p.permission,
                                  columns: [],
                                  set: {},
                                }
                              : p.permission,
                          }),
                        ) ?? []
                    );
                  }),
                ),
              ),
            );
          }
        }
      }

      if (!args.length) {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: `No table permission with role ${fromRole}. Skip action.`,
        });

        options?.onSuccess?.({}, {} as any, null, {} as any);
        return;
      }

      return mutate(
        {
          query: {
            type: 'bulk',
            args,
            resource_version,
          },
        },
        {
          ...options,
          onError: (error, variables, onMutateResult, context) => {
            hasuraToast({
              type: 'error',
              title: errorTitle,
              message: getErrorMessage(error),
            });
            options?.onError?.(error, variables, onMutateResult, context);
          },
          onSuccess: (data, variables, onMutateResult, context) => {
            hasuraToast({
              type: 'success',
              title: 'Permissions copied!',
              message: `Permissions from role ${fromRole} were copied to role ${toRoles}`,
            });

            options?.onSuccess?.(data, variables, onMutateResult, context);
          },
        },
      );
    },
    [mutation],
  );

  return {
    ...mutation,
    bulkCopyPermissionsByRole,
  };
};
