import { Action, Permission } from './types';
import { usePermissionsFormContext } from '../hooks/usePermissionForm';
import {
  PermissionsLegend,
  PermissionsTableRow,
  PermissionsTableView,
} from '../../PermissionsTable';

export type PermissionsTableProps = {
  allowedActions: Action[];
  permissions: Permission[];
};

const actions = ['insert', 'select', 'update', 'delete'];

const columns = actions.map((action) => ({
  key: action,
  label: action.toUpperCase(),
}));

export const PermissionsTable = ({
  allowedActions,
  permissions,
}: PermissionsTableProps) => {
  const {
    setActivePermission,
    activePermission,
    unsetActivePermission,
    permissionAccess,
    setNewRoleName,
  } = usePermissionsFormContext();

  const rows: PermissionsTableRow[] = permissions.map((permission, index) => ({
    roleName: permission.roleName,
    roleCell: permission.isNew
      ? {
          isNewRole: true,
          newRoleValue: permission.roleName,
          onNewRoleValueChange: setNewRoleName,
        }
      : undefined,
    cells: Object.fromEntries(
      actions.map((actionName) => {
        const action = allowedActions.find(
          (allowedAction) => allowedAction === actionName,
        );
        return [
          actionName,
          {
            isEditable: Boolean(action),
            access: permissionAccess(action, permission),
            isCurrentEdit:
              activePermission === index && permission.action === action,
            testId: `${permission.roleName}-${actionName}-permissions-cell`,
            onClick: () => {
              // Close form if user clicks cell for empty permission
              if (
                activePermission !== index &&
                permission.isNew &&
                permission.roleName === ''
              ) {
                unsetActivePermission();
              }
              // Focus on input so that user can enter new role name
              if (permission.isNew && permission.roleName === '') {
                document.getElementById('new-role-input')?.focus();
              } else {
                setActivePermission(index);
              }
            },
          },
        ];
      }),
    ),
  }));

  return (
    <div data-testid="permissions-table">
      <PermissionsLegend className="mb-4" />
      <PermissionsTableView columns={columns} rows={rows} />
    </div>
  );
};
