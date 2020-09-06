import { findActionPermission } from '../utils';
import {
  getCreateActionPermissionQuery,
  getDropActionPermissionQuery,
} from '../../../../metadata/queryUtils';

export const getActionPermissionQueries = (
  permissionEdit,
  allPermissions,
  actionName
) => {
  const { role, newRole, filter } = permissionEdit;

  const upQueries = [];
  const downQueries = [];

  const permRole = (newRole || role).trim();

  const existingPerm = findActionPermission(allPermissions, permRole);

  if (newRole || (!newRole && !existingPerm)) {
    upQueries.push(
      getCreateActionPermissionQuery(
        {
          role: permRole,
          filter,
        },
        actionName
      )
    );
    downQueries.push(getDropActionPermissionQuery(permRole, actionName));
  }

  if (existingPerm) {
    upQueries.push(getDropActionPermissionQuery(permRole, actionName));
    upQueries.push(
      getCreateActionPermissionQuery({ role: permRole, filter }, actionName)
    );
    downQueries.push(getDropActionPermissionQuery(permRole, actionName));
    upQueries.push(
      getCreateActionPermissionQuery(
        { role: permRole, filter: existingPerm.definition.select.filter },
        actionName
      )
    );
  }

  return {
    upQueries,
    downQueries,
  };
};
