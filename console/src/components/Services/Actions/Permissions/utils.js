import { findActionPermission } from '../utils';
import {
  getCreateActionPermissionQuery,
  getDropActionPermissionQuery,
} from '../../../../metadata/queryUtils';
import Migration from '../../../../utils/migration/Migration';

export const getActionPermissionMigration = (
  permissionEdit,
  allPermissions,
  actionName
) => {
  const { role, newRole, filter } = permissionEdit;
  const permRole = (newRole || role).trim();
  const existingPerm = findActionPermission(allPermissions, permRole);
  const migration = new Migration();

  if (newRole || (!newRole && !existingPerm)) {
    const createActionPermission = getCreateActionPermissionQuery(
      {
        role: permRole,
        filter,
      },
      actionName
    );
    const dropActionPermission = getDropActionPermissionQuery(
      permRole,
      actionName
    );

    migration.add(createActionPermission, dropActionPermission);

    return migration;
  }
  migration.add(
    getDropActionPermissionQuery(permRole, actionName),
    getDropActionPermissionQuery(permRole, actionName)
  );

  migration.add(
    getCreateActionPermissionQuery({ role: permRole, filter }, actionName)
  ); // TODO Down queries
  migration.add(
    getCreateActionPermissionQuery(
      { role: permRole, filter: existingPerm.definition.select.filter },
      actionName
    )
  ); // TODO Down queries

  return migration;
};
