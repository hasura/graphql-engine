import { findActionPermission } from '../utils';
import Migration from '../../../../utils/migration/Migration';
import {
  getCreateActionPermissionQuery,
  getDropActionPermissionQuery,
} from '../../../../metadata/queryUtils';

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
    migration.add(
      getCreateActionPermissionQuery(
        {
          role: permRole,
          filter,
        },
        actionName
      ),
      getDropActionPermissionQuery(permRole, actionName)
    );
  }

  if (existingPerm) {
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
  }

  return migration;
};
