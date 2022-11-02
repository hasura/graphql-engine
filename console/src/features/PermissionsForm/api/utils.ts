import produce from 'immer';

import { allowedMetadataTypes } from '@/features/MetadataAPI';

import { AccessType } from '../types';
import { PermissionsSchema } from '../utils';

interface PermissionArgs {
  columns: string[];
  presets?: Record<string, string | number>;
  computed_fields: string[];
  backend_only?: boolean;
  allow_aggregations?: boolean;
  check: Record<string, any>;
  filter: Record<string, any>;
  limit?: number;
}

/**
 * creates the permissions object for the server
 */
const createPermission = (formData: PermissionsSchema) => {
  // presets need reformatting for server
  const presets = formData.presets?.reduce((acc, preset) => {
    if (preset.columnValue) {
      acc[preset.columnName] = preset.columnValue;
    }

    return acc;
  }, {} as Record<string, string | number>);

  // columns need reformatting for server
  const columns = Object.entries(formData.columns)
    .filter(({ 1: value }) => value)
    .map(([key]) => key);

  // in row permissions builder an extra input is rendered automatically
  // this will always be empty and needs to be removed
  const filter = Object.entries(formData.filter).reduce<Record<string, any>>(
    (acc, [operator, value]) => {
      if (operator === '_and' || operator === '_or') {
        const newValue = (value as any[])?.slice(0, -1);
        acc[operator] = newValue;
        return acc;
      }

      acc[operator] = value;
      return acc;
    },
    {}
  );
  // return permissions object for args
  const permissionObject: PermissionArgs = {
    columns,
    presets,
    computed_fields: [],
    backend_only: formData.backendOnly,
    allow_aggregations: formData.aggregationEnabled,
    check: formData.check,
    filter,
  };

  if (formData.rowCount && formData.rowCount !== '0') {
    permissionObject.limit = parseInt(formData.rowCount, 10);
  }

  return permissionObject;
};

export interface CreateInsertArgs {
  dataSourceName: string;
  table: unknown;
  queryType: string;
  role: string;
  accessType: AccessType;
  formData: PermissionsSchema;
  existingPermissions: ExistingPermission[];
  driver: string;
}

interface ExistingPermission {
  table: unknown;
  role: string;
  queryType: string;
}
/**
 * creates the insert arguments to update permissions
 * adds cloned permissions
 * and creates drop arguments where permissions already exist
 */
export const createInsertArgs = ({
  dataSourceName,
  table,
  queryType,
  role,
  formData,
  existingPermissions,
  driver,
}: CreateInsertArgs) => {
  const permission = createPermission(formData);

  // create args object with args from form
  const initialArgs = [
    {
      type: `${driver}_create_${queryType}_permission` as allowedMetadataTypes,
      args: {
        table,
        role,
        permission,
        source: dataSourceName,
      },
    },
  ];

  const args = produce(initialArgs, draft => {
    // determine if args from form already exist
    const permissionExists = existingPermissions.find(
      existingPermission =>
        JSON.stringify(existingPermission.table) === JSON.stringify(table) &&
        existingPermission.role === role &&
        existingPermission.queryType === queryType
    );

    // if the permission already exists it needs to be dropped
    if (permissionExists) {
      draft.unshift({
        type: `${driver}_drop_${queryType}_permission` as allowedMetadataTypes,
        args: {
          table,
          role,
          source: dataSourceName,
        },
      } as typeof initialArgs[0]);
    }

    // last item is always empty default
    const clonedPermissions = formData?.clonePermissions?.slice(0, -1);

    if (clonedPermissions?.length) {
      clonedPermissions.forEach(clonedPermission => {
        // if permissions are being applied to a different table
        // columns and presets should be blank
        const permissionWithColumnsAndPresetsRemoved = produce(
          permission,
          d => {
            if (clonedPermission.tableName !== table) {
              d.columns = [];
              d.presets = {};
            }

            return d;
          }
        );
        // add each closed permission to args
        draft.push({
          type: `${driver}_create_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
          args: {
            table: clonedPermission.tableName || '',
            role: clonedPermission.roleName || '',
            permission: permissionWithColumnsAndPresetsRemoved,
            source: dataSourceName,
          },
        });

        // determined if the cloned permission already exists
        const clonedPermissionExists = existingPermissions.find(
          existingPermission =>
            JSON.stringify(existingPermission.table) ===
              JSON.stringify(clonedPermission.tableName) &&
            existingPermission.role === clonedPermission.roleName &&
            existingPermission.queryType === clonedPermission.queryType
        );

        // if it already exists drop it
        if (clonedPermissionExists) {
          draft.unshift({
            type: `${driver}_drop_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
            args: {
              table: clonedPermission.tableName,
              role: clonedPermission.roleName,
              source: dataSourceName,
            },
          } as typeof initialArgs[0]);
        }
      });
    }
  });

  return args;
};
