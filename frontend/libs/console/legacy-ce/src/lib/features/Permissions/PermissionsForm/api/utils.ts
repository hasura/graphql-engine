import produce from 'immer';

import { allowedMetadataTypes } from '@/features/MetadataAPI';

import { AccessType } from '../../types';
import { PermissionsSchema, Presets } from '../../schema';
import { areTablesEqual } from '@/features/hasura-metadata-api';
import { Table } from '@/features/hasura-metadata-types';
import { getTableDisplayName } from '@/features/DatabaseRelationships';

type SelectPermissionMetadata = {
  columns: string[];
  presets: Presets;
  filter: Record<string, any>;
  allow_aggregations?: boolean;
  limit?: number;
  query_root_fields?: string[];
  subscription_root_fields?: string[];
};

const createSelectObject = (input: PermissionsSchema) => {
  if (input.queryType === 'select') {
    const columns = Object.entries(input.columns)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);

    // in row permissions builder an extra input is rendered automatically
    // this will always be empty and needs to be removed

    const filter = Object.entries(input.filter).reduce<Record<string, any>>(
      (acc, [operator, value]) => {
        if (operator === '_and' || operator === '_or') {
          const filteredEmptyObjects = (value as any[]).filter(
            p => Object.keys(p).length !== 0
          );
          acc[operator] = filteredEmptyObjects;
          return acc;
        }

        acc[operator] = value;
        return acc;
      },
      {}
    );

    const permissionObject: SelectPermissionMetadata = {
      columns,
      filter,
      presets: [],
      allow_aggregations: input.aggregationEnabled,
    };

    if (input.query_root_fields) {
      permissionObject.query_root_fields = input.query_root_fields;
    }
    if (input.subscription_root_fields) {
      permissionObject.subscription_root_fields =
        input.subscription_root_fields;
    }

    if (input.rowCount && input.rowCount !== '0') {
      permissionObject.limit = parseInt(input.rowCount, 10);
    }

    return permissionObject;
  }

  throw new Error('Case not handled');
};

/**
 * creates the permissions object for the server
 */
const createPermission = (formData: PermissionsSchema) => {
  switch (formData.queryType) {
    case 'select':
      return createSelectObject(formData);
    case 'insert':
      throw new Error('Case not handled');
    case 'update':
      throw new Error('Case not handled');
    case 'delete':
      throw new Error('Case not handled');
    default:
      throw new Error('Case not handled');
  }
};

export interface CreateInsertArgs {
  dataSourceName: string;
  table: unknown;
  tables: Table[];
  queryType: any;
  role: string;
  accessType: AccessType;
  formData: PermissionsSchema;
  existingPermissions: ExistingPermission[];
  driver: string;
}

export interface ExistingPermission {
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
  tables,
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
        areTablesEqual(existingPermission.table, table) &&
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
      } as (typeof initialArgs)[0]);
    }

    // last item is always empty default
    const clonedPermissions = formData?.clonePermissions?.slice(0, -1);

    if (clonedPermissions?.length) {
      clonedPermissions.forEach(clonedPermission => {
        const clonedPermissionTable = tables.find(
          t => getTableDisplayName(t) === clonedPermission.tableName
        );
        // if permissions are being applied to a different table
        // columns and presets should be blank
        const permissionWithColumnsAndPresetsRemoved = produce(
          permission,
          d => {
            if (!areTablesEqual(clonedPermissionTable, table)) {
              d.columns = [];
              d.presets = [];
            }

            return d;
          }
        );
        // add each closed permission to args
        draft.push({
          type: `${driver}_create_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
          args: {
            table: clonedPermissionTable || '',
            role: clonedPermission.roleName || '',
            permission: permissionWithColumnsAndPresetsRemoved,
            source: dataSourceName,
          },
        });

        // determined if the cloned permission already exists
        const clonedPermissionExists = existingPermissions.find(
          existingPermission =>
            areTablesEqual(existingPermission.table, clonedPermissionTable) &&
            existingPermission.role === clonedPermission.roleName &&
            existingPermission.queryType === clonedPermission.queryType
        );

        // if it already exists drop it
        if (clonedPermissionExists) {
          draft.unshift({
            type: `${driver}_drop_${clonedPermission.queryType}_permission` as allowedMetadataTypes,
            args: {
              table: clonedPermissionTable,
              role: clonedPermission.roleName,
              source: dataSourceName,
            },
          } as (typeof initialArgs)[0]);
        }
      });
    }
  });

  return args;
};
