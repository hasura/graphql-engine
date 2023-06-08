import { forwardRef } from 'react';
import { Action, Permission } from './types';
import { PermissionsRowName } from './PermissionsRowName';
import { PermissionAccessCell } from './PermissionAccessCell';
import { usePermissionsFormContext } from '../hooks/usePermissionForm';

type PermissionsRowProps = {
  permission: Permission;
  index: number;
  actions: string[];
  allowedActions: Action[];
};

export const PermissionsRow = forwardRef<HTMLInputElement, PermissionsRowProps>(
  ({ permission, actions, allowedActions, index }, ref) => {
    const {
      setActivePermission,
      activePermission,
      unsetActivePermission,
      permissionAccess,
    } = usePermissionsFormContext();
    return (
      <tr className="group divide-x divide-gray-300">
        <PermissionsRowName
          ref={ref}
          roleName={permission.roleName}
          isNew={permission.isNew}
        />
        {actions.map(actionName => {
          const action = allowedActions.find(
            allowedAction => allowedAction === actionName
          );
          return (
            <PermissionAccessCell
              key={actionName}
              isEditable={Boolean(action)}
              access={permissionAccess(action, permission)}
              isCurrentEdit={
                activePermission === index && permission.action === action
              }
              data-testid={`${permission.roleName}-${actionName}-permissions-cell`}
              onClick={() => {
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
                  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                  // @ts-ignore
                  ref.current?.focus();
                } else {
                  setActivePermission(index);
                }
              }}
            />
          );
        })}
      </tr>
    );
  }
);
