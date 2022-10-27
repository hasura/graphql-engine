import produce from 'immer';

import { allowedMetadataTypes } from '@/features/MetadataAPI';

import { AccessType, FormOutput } from '../types';

interface PermissionArgs {
  columns: string[];
  presets?: Record<string, string | number>;
  computed_fields: string[];
  backend_only: boolean;
  allow_aggregations: boolean;
  check: Record<string, unknown>;
  filter: Record<string, unknown>;
  limit?: number;
}

/**
 * creates the permissions object for the server
 */
const createPermission = (formData: FormOutput) => {
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

  // return permissions object for args
  const permissionObject: PermissionArgs = {
    columns,
    presets,
    computed_fields: [],
    backend_only: formData.backendOnly,
    allow_aggregations: formData.aggregationEnabled,
    check: formData.check,
    filter: formData.filter,
  };

  if (formData.rowCount && formData.rowCount !== '0') {
    permissionObject.limit = parseInt(formData.rowCount, 10);
  }

  return permissionObject;
};

export interface CreateInsertArgs {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
  queryType: string;
  role: string;
  accessType: AccessType;
  formData: FormOutput;
  existingPermissions: ExistingPermission[];
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
  currentSource,
  dataSourceName,
  table,
  queryType,
  role,
  formData,
  existingPermissions,
}: CreateInsertArgs) => {
  const permission = createPermission(formData);

  // create args object with args from form
  const initialArgs = [
    {
      type: `${currentSource}_create_${queryType}_permission` as allowedMetadataTypes,
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
        type: `${currentSource}_drop_${queryType}_permission` as allowedMetadataTypes,
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
          type: `${currentSource}_create_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
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
            type: `${currentSource}_drop_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
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
