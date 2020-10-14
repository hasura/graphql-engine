import {
  getCreateActionPermissionQuery,
  getDropActionPermissionQuery,
} from '../../../Common/utils/v1QueryUtils';
import { findRemoteSchemaPermission } from '../utils';


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
      getCreateActionPermissionQuery(
        {
          role: permRole,
          filter,
        },
        remoteSchemaName
      )
    );
    downQueries.push(getDropActionPermissionQuery(permRole, remoteSchemaName));
  }

  if (existingPerm) {
    upQueries.push(getDropActionPermissionQuery(permRole, remoteSchemaName));
    upQueries.push(
      getCreateActionPermissionQuery(
        { role: permRole, filter },
        remoteSchemaName
      )
    );
    downQueries.push(getDropActionPermissionQuery(permRole, remoteSchemaName));
    upQueries.push(
      getCreateActionPermissionQuery(
        { role: permRole, filter: existingPerm.definition.select.filter },
        remoteSchemaName
      )
    );
  }

  return {
    upQueries,
    downQueries,
  };
};
