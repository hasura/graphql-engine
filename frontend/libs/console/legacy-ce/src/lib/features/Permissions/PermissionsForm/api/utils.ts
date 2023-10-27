import produce, { original } from 'immer';

import { allowedMetadataTypes } from '../../../MetadataAPI';

import { AccessType, QueryType } from '../../types';
import { PermissionsSchema } from '../../schema';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { Table } from '../../../hasura-metadata-types';
import { getTableDisplayName } from '../../../DatabaseRelationships';
import { inputValidationSchema } from '../../../../components/Services/Data/TablePermissions/InputValidation/InputValidation';
import { z } from 'zod';

const formatFilterValues = (formFilter: Record<string, any>[] = []) => {
  return Object.entries(formFilter).reduce<Record<string, any>>(
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
};

type SelectPermissionMetadata = {
  columns: string[];
  set: Record<string, any>;
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

    // Input may be undefined
    const filter = formatFilterValues(input.filter);

    const permissionObject: SelectPermissionMetadata = {
      columns,
      filter,
      set: {},
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

type InsertPermissionMetadata = {
  columns: string[];
  check: Record<string, any>;
  allow_upsert: boolean;
  backend_only?: boolean;
  set: Record<string, any>;
  validate_input?: z.infer<typeof inputValidationSchema>;
};

const createInsertObject = (input: PermissionsSchema) => {
  if (input.queryType === 'insert') {
    const columns = Object.entries(input.columns)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);

    const set =
      input?.presets?.reduce((acc, preset) => {
        if (preset.columnName === 'default') return acc;
        return { ...acc, [preset.columnName]: preset.columnValue };
      }, {}) ?? {};

    const permissionObject: InsertPermissionMetadata = {
      columns,
      check: input.check,
      allow_upsert: true,
      set,
      backend_only: input.backendOnly,
      validate_input: input.validateInput,
    };

    return permissionObject;
  }

  throw new Error('Case not handled');
};

export type DeletePermissionMetadata = {
  columns?: string[];
  set?: Record<string, any>;
  backend_only: boolean;
  filter: Record<string, any>;
};

const createDeleteObject = (input: PermissionsSchema) => {
  if (input.queryType === 'delete') {
    // Input may be undefined
    const filter = formatFilterValues(input.filter);

    const permissionObject: DeletePermissionMetadata = {
      backend_only: input.backendOnly || false,
      filter,
    };

    return permissionObject;
  }

  throw new Error('Case not handled');
};

type UpdatePermissionMetadata = {
  columns: string[];
  filter: Record<string, any>; // filter is PRE
  check?: Record<string, any>; // check is POST
  backend_only?: boolean;
  set: Record<string, any>;
  validate_input?: z.infer<typeof inputValidationSchema>;
};

const createUpdateObject = (input: PermissionsSchema) => {
  if (input.queryType === 'update') {
    const columns = Object.entries(input.columns)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);

    const filter = formatFilterValues(input.filter);

    const check = formatFilterValues(input.check);
    const set =
      input?.presets?.reduce((acc, preset) => {
        if (preset.columnName === 'default') return acc;
        const isNumber = !isNaN(Number(preset.columnValue));
        return {
          ...acc,
          [preset.columnName]: isNumber
            ? Number(preset.columnValue)
            : preset.columnValue,
        };
      }, {}) ?? {};

    const permissionObject: UpdatePermissionMetadata = {
      columns,
      filter,
      check,
      set,
      backend_only: input.backendOnly,
      validate_input: input.validateInput,
    };

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
      return createInsertObject(formData);
    case 'update':
      return createUpdateObject(formData);
    case 'delete':
      return createDeleteObject(formData);
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
 * When cloning permissions we have to transform the payload between filter and checks.
 * The input per type is:
 * select uses filter
 * delete uses filter
 * insert uses check
 * update uses filter(for pre-check) and check (for post-check)
 *
 * When cloning between the permissions type we need to swap these out based on with way we are cloning
 */
const getPermissionsWithMappedRowPermissions = (
  permissionsObject: any,
  mainQueryType: QueryType,
  clonedQueryType: string
): {
  filter?: Record<string, any>;
  check?: Record<string, any>;
  columns?: string[];
} => {
  const clone: {
    filter?: Record<string, any>;
    check?: Record<string, any>;
    columns?: string[];
  } = {
    filter: permissionsObject.filter,
    check: permissionsObject.check,
  };

  if (
    (mainQueryType === 'select' && clonedQueryType === 'insert') ||
    (mainQueryType === 'select' && clonedQueryType === 'update') ||
    (mainQueryType === 'delete' && clonedQueryType === 'insert') ||
    (mainQueryType === 'delete' && clonedQueryType === 'update')
  ) {
    clone.check = permissionsObject.filter;
  }

  if (
    (mainQueryType === 'update' && clonedQueryType === 'select') ||
    (mainQueryType === 'update' && clonedQueryType === 'delete') ||
    (mainQueryType === 'insert' && clonedQueryType === 'select') ||
    (mainQueryType === 'insert' && clonedQueryType === 'delete') ||
    (mainQueryType === 'insert' && clonedQueryType === 'update')
  ) {
    clone.filter = permissionsObject.check;
  }

  if (mainQueryType === 'delete')
    clone.columns = permissionsObject.columns || [];

  return { filter: clone.filter, check: clone.check, columns: clone.columns };
};

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
        comment: formData.comment,
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
              d.set = {};
            }

            const newValues = {
              ...getPermissionsWithMappedRowPermissions(
                original(d),
                queryType,
                clonedPermission.queryType
              ),
            };
            // eslint-disable-next-line @typescript-eslint/ban-ts-comment
            // @ts-ignore
            d.filter = newValues.filter;
            // eslint-disable-next-line @typescript-eslint/ban-ts-comment
            // @ts-ignore
            d.check = newValues.check;
            d.columns = newValues.columns;
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
            comment: '',
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
