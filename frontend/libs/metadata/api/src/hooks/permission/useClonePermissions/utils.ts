import {
  DataQueryType,
  InsertPermissionDefinition,
  SelectPermissionDefinition,
  Source,
  Table,
  TablePermission,
  TablePermissionDefinition,
  UpdatePermissionDefinition,
} from '@hasura/shared/types';
import { ClonePermissionSchema } from './schema';
import { TMigrationSingleQuery } from '../../../api';
import {
  areTablesEqual,
  getDriverPrefix,
  MetadataSelectors,
} from '@hasura/metadata/helpers';

interface ClonePermissionsArgs {
  source: Source;
  table: unknown;
  queryType: DataQueryType;
  to: ClonePermissionSchema;
  from: TablePermission;
  resourceVersion?: number;
}

/**
 * creates arguments to cloned permissions
 */
export const getClonePermissionsArgs = ({
  source,
  table,
  from,
  queryType,
  to,
}: ClonePermissionsArgs): TMigrationSingleQuery[] => {
  // last item is always empty default
  const clonedPermissions = to;
  if (!clonedPermissions?.length) {
    return [];
  }

  const prefix = getDriverPrefix(source.kind);
  // create args object with args from form
  const args: TMigrationSingleQuery[] = [];

  clonedPermissions.forEach((clonedPermission) => {
    if (
      !clonedPermission.queryType ||
      !clonedPermission.roleName ||
      !clonedPermission.table
    ) {
      return;
    }

    const doesExist =
      MetadataSelectors.findPermissionFromTables(
        source.tables,
        clonedPermission.table as Table,
        clonedPermission.queryType,
        clonedPermission.roleName,
      ) !== undefined;

    if (doesExist) {
      // determined if the cloned permission already exists
      args.push({
        type: `${prefix}_drop_${clonedPermission.queryType}_permission` as const,
        args: {
          table: clonedPermission.table,
          role: clonedPermission.roleName,
          source: source.name,
        },
      });
    }

    // if permissions are being applied to a different table
    // columns and presets should be blank
    const clearColumnsAndPresets = !areTablesEqual(
      clonedPermission.table as Table,
      table as Table,
    );

    const { set, ...fromPermission } =
      from.permission as UpdatePermissionDefinition;
    const newValues = getPermissionsWithMappedRowPermissions(
      fromPermission,
      queryType,
      clonedPermission.queryType,
    );

    const permissionWithColumnsAndPresetsRemoved = {
      ...fromPermission,
      ...newValues,
      ...(clearColumnsAndPresets
        ? { columns: [], set: {} }
        : {
            set: set
              ? Object.entries(set).reduce(
                  (acc, [key, value]) => {
                    if (key) {
                      acc[key] = value;
                    }

                    return acc;
                  },
                  {} as Record<string, any>,
                )
              : undefined,
          }),
    };

    // add each closed permission to args
    args.push({
      type: `${prefix}_create_${clonedPermission.queryType}_permission` as const,
      args: {
        table: clonedPermission.table,
        role: clonedPermission.roleName,
        permission: permissionWithColumnsAndPresetsRemoved,
        source: source.name,
        comment: '',
      },
    });
  });

  return args;
};

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
type GetPermissionsWithMappedRowPermissionsResult = {
  filter?: Record<string, any>;
  check?: Record<string, any>;
  columns?: string[] | '*';
};
const getPermissionsWithMappedRowPermissions = (
  permissionsObject: TablePermissionDefinition,
  fromQueryType: DataQueryType,
  toQueryType: DataQueryType,
): GetPermissionsWithMappedRowPermissionsResult => {
  const clone: GetPermissionsWithMappedRowPermissionsResult = {
    filter: (permissionsObject as SelectPermissionDefinition).filter,
    check: (permissionsObject as InsertPermissionDefinition).check,
  };

  if (
    ((fromQueryType === 'select' && toQueryType === 'insert') ||
      (fromQueryType === 'select' && toQueryType === 'update') ||
      (fromQueryType === 'delete' && toQueryType === 'insert') ||
      (fromQueryType === 'delete' && toQueryType === 'update')) &&
    'filter' in permissionsObject
  ) {
    clone.check = permissionsObject.filter;
  }

  if (
    ((fromQueryType === 'update' && toQueryType === 'select') ||
      (fromQueryType === 'update' && toQueryType === 'delete') ||
      (fromQueryType === 'insert' && toQueryType === 'select') ||
      (fromQueryType === 'insert' && toQueryType === 'delete') ||
      (fromQueryType === 'insert' && toQueryType === 'update')) &&
    'check' in permissionsObject
  ) {
    clone.filter = permissionsObject.check;
  }

  if (toQueryType !== 'delete') {
    clone.columns =
      (permissionsObject as SelectPermissionDefinition).columns ?? [];
  }

  return { filter: clone.filter, check: clone.check, columns: clone.columns };
};
