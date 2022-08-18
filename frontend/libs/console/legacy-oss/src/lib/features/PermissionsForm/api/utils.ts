import produce from 'immer';

import { allowedMetadataTypes } from '@/features/MetadataAPI';

import { FormOutput } from '../types';

export const driverPrefixes = {
  postgres: 'pg',
  mysql: 'mysql',
  mssql: 'mssql',
  bigquery: 'bigquery',
  citus: 'citus',
} as const;

type DriverPrefixKeys = keyof typeof driverPrefixes;
type DriverPrefixValues = typeof driverPrefixes[DriverPrefixKeys];

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
  return {
    columns,
    presets,
    computed_fields: [],
    backend_only: formData.backendOnly,
    limit: parseInt(formData.rowCount, 10),
    allow_aggregations: formData.aggregationEnabled,
    check: JSON.parse(formData.check || '{}'),
    filter: JSON.parse(formData.filter || '{}'),
  };
};

export interface CreateInsertArgs {
  driverPrefix: DriverPrefixValues;
  database: string;
  table: string;
  queryType: string;
  role: string;
  formData: FormOutput;
  existingPermissions: ExistingPermission[];
}

interface ExistingPermission {
  table: string;
  role: string;
  queryType: string;
}

/**
 * creates the insert arguments to update permissions
 * adds cloned permissions
 * and creates drop arguments where permissions already exist
 */
export const createInsertArgs = ({
  driverPrefix,
  database,
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
      type: `${driverPrefix}_create_${queryType}_permission` as allowedMetadataTypes,
      args: {
        table,
        role,
        permission,
        source: database,
      },
    },
  ];

  const args = produce(initialArgs, draft => {
    // determine if args from form already exist
    const permissionExists = existingPermissions.find(
      existingPermission =>
        existingPermission.table === table &&
        existingPermission.role === role &&
        existingPermission.queryType === queryType
    );

    // if the permission already exists it needs to be dropped
    if (permissionExists) {
      draft.unshift({
        type: `${driverPrefix}_drop_${queryType}_permission` as allowedMetadataTypes,
        args: {
          table,
          role,
          source: database,
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
          type: `${driverPrefix}_create_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
          args: {
            table: clonedPermission.tableName || '',
            role: clonedPermission.roleName || '',
            permission: permissionWithColumnsAndPresetsRemoved,
            source: database,
          },
        });

        // determined if the cloned permission already exists
        const clonedPermissionExists = existingPermissions.find(
          existingPermission =>
            existingPermission.table === clonedPermission.tableName &&
            existingPermission.role === clonedPermission.roleName &&
            existingPermission.queryType === clonedPermission.queryType
        );

        // if it already exists drop it
        if (clonedPermissionExists) {
          draft.unshift({
            type: `${driverPrefix}_drop_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
            args: {
              table: clonedPermission.tableName,
              role: clonedPermission.roleName,
              source: database,
            },
          } as typeof initialArgs[0]);
        }
      });
    }
  });

  return args;
};
