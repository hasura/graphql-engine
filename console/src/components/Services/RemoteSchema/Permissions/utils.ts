import { findRemoteSchemaPermission } from '../utils';

export const getCreateRemoteSchemaPermissionQuery = (
  def: { role: string; filter: any },
  remoteSchemaName: string,
  permissionEdit: any
) => {
  return {
    type: 'add_remote_schema',
    args: {
      remote_schema: remoteSchemaName,
      role: def.role,
      definition: {
        schema: permissionEdit.schemaDefinition,
      },
    },
  };
};

export const getDropRemoteSchemaPermissionQuery = (
  role: string,
  remoteSchemaName: string
) => {
  return {
    type: 'drop_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
    },
  };
};

// TODO: imports
export const getRemoteSchemaPermissionQueries = (
  permissionEdit,
  allPermissions,
  remoteSchemaName
) => {
  const { role, newRole, filter } = permissionEdit;

  const upQueries = [];
  const downQueries = [];

  const permRole = (newRole || role).trim();

  const existingPerm = findRemoteSchemaPermission(allPermissions, permRole);

  if (newRole || (!newRole && !existingPerm)) {
    upQueries.push(
      getCreateRemoteSchemaPermissionQuery(
        {
          role: permRole,
          filter,
        },
        remoteSchemaName,
        permissionEdit
      )
    );
    downQueries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
  }

  if (existingPerm) {
    upQueries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
    upQueries.push(
      getCreateRemoteSchemaPermissionQuery(
        { role: permRole, filter },
        remoteSchemaName,
        permissionEdit
      )
    );
    downQueries.push(
      getDropRemoteSchemaPermissionQuery(permRole, remoteSchemaName)
    );
    upQueries.push(
      getCreateRemoteSchemaPermissionQuery(
        { role: permRole, filter: existingPerm.definition.select.filter },
        remoteSchemaName,
        permissionEdit
      )
    );
  }

  return {
    upQueries,
    downQueries,
  };
};
