import { useForm, useFormContext } from 'react-hook-form';
import { LogicalModelPermissionsState } from '../LogicalModelPermissions';
import {
  AccessType,
  Action,
  LogicalModelWithPermissions,
  Permission,
  RowSelectPermissionsType,
} from '../components/types';
import isEmpty from 'lodash/isEmpty';
import { permissionColumnAccess, permissionRowAccess } from '../utils';

export function useLogicalModelPermissionsForm(
  logicalModel: LogicalModelWithPermissions | undefined,
  allRoles: string[]
) {
  const defaultPermissions: Permission[] = allRoles.map(role => {
    const savedPermissionForRole = logicalModel?.select_permissions?.find(
      select_perm => select_perm.role === role
    );

    if (!savedPermissionForRole)
      return {
        roleName: role,
        filter: {},
        columns: [],
        action: 'select' as const,
        isNew: false,
        source: logicalModel?.source.name || '',
      };

    return {
      roleName: savedPermissionForRole.role,
      filter: savedPermissionForRole.permission.filter,
      columns: savedPermissionForRole.permission.columns,
      action: 'select' as const,
      isNew: false,
      source: logicalModel?.source.name ?? '',
    };
  });

  const methods = useForm<LogicalModelPermissionsState>({
    defaultValues: {
      activePermission: null,
      rowSelectPermissions: 'without_filter',
      /**
       * Push an empty record at the end for "Add New Role" row
       */
      permissions: [
        ...defaultPermissions,
        {
          roleName: '',
          filter: {},
          columns: [],
          action: 'select' as const,
          isNew: true,
          source: logicalModel?.source.name || '',
        },
      ],
      columns: logicalModel?.fields?.map(field => field.name) ?? [],
    },
  });
  return methods;
}

export function usePermissionsFormContext() {
  const { setValue, watch } = useFormContext<LogicalModelPermissionsState>();
  return {
    permissions: watch('permissions'),
    setPermission: (roleName: string, filter: Record<string, any>) => {
      const permissions = watch('permissions');
      setValue(
        'permissions',
        permissions.map(permission => {
          if (permission.roleName === roleName) {
            return {
              ...permission,
              filter,
            };
          }
          return permission;
        })
      );
    },
    rowSelectPermissions: watch('rowSelectPermissions'),
    setRowSelectPermissions: (rowSelectPermissions: RowSelectPermissionsType) =>
      setValue('rowSelectPermissions', rowSelectPermissions),
    columns: watch('columns'),
    toggleColumn: (permission: Permission, column: string) => {
      const permissions = watch('permissions');
      setValue(
        'permissions',
        permissions.map(p => {
          if (
            p.roleName === permission.roleName &&
            p.source === permission.source
          ) {
            const columns = p.columns.includes(column)
              ? p.columns.filter(c => c !== column)
              : [...p.columns, column];
            return {
              ...p,
              columns,
            };
          }
          return p;
        })
      );
    },
    toggleAllColumns: (permission: Permission) => {
      const permissions = watch('permissions');
      setValue(
        'permissions',
        permissions.map(p => {
          if (
            p.roleName === permission.roleName &&
            p.source === permission.source
          ) {
            const columns =
              p.columns.length === watch('columns').length
                ? []
                : watch('columns');
            return {
              ...p,
              columns,
            };
          }
          return p;
        })
      );
    },
    columnPermissionsStatus: (
      permission: Permission
    ): '' | 'No columns' | 'All columns' | 'Partial columns' => {
      if (!permission) {
        return '';
      }
      if (permission.columns.length === 0) {
        return 'No columns';
      }
      if (permission.columns.length === watch('columns').length) {
        return 'All columns';
      }
      return 'Partial columns';
    },
    setActivePermission: (index: number) => {
      const permission = watch('permissions')[index];
      const rowSelectPermissions: RowSelectPermissionsType =
        permission.isNew || isEmpty(permission.filter)
          ? 'without_filter'
          : 'with_custom_filter';

      setValue('activePermission', index);
      setValue('rowSelectPermissions', rowSelectPermissions);
    },
    unsetActivePermission: () => setValue('activePermission', null),
    activePermission: watch('activePermission'),
    setNewRoleName: (roleName: string) => {
      const permissions = watch('permissions');
      setValue(
        'permissions',
        permissions.map(permission => {
          if (permission.isNew) {
            return {
              ...permission,
              roleName,
            };
          }
          return permission;
        })
      );
    },
    /**
     * Returns the access type of the permission for the given action
     */
    permissionAccess: (
      action: Action | undefined,
      permission: Permission
    ): AccessType => {
      if (action !== 'select') {
        return 'noAccess';
      }
      const rowAccess = permissionRowAccess(permission);
      const columnAccess = permissionColumnAccess(permission, watch('columns'));
      if (rowAccess === 'noAccess' && columnAccess === 'noAccess') {
        return 'noAccess';
      }
      if (rowAccess === 'fullAccess' && columnAccess === 'fullAccess') {
        return 'fullAccess';
      }
      return 'partialAccess';
    },
  };
}
