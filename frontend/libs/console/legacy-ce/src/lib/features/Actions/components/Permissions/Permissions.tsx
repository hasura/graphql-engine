import { useState } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useDocumentTitle } from '@hasura/shared/hooks';
import PermissionEditor, { ActionPermissionState } from './PermissionEditor';
import { useAppContext } from '@hasura/shared/context';
import { useCurrentActionContext } from '../../context';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { PermissionsLegend } from './PermissionsLegend';
import {
  PermissionsTableView,
  PermissionsTableRow,
} from '../../../Permissions/PermissionsTable';
import { Flex } from '@radix-ui/themes';

const PERMISSION_COLUMN = { key: 'permission', label: 'Permission' };

const Permissions = () => {
  const { readOnlyMode } = useAppContext();
  const { currentAction, metadata } = useCurrentActionContext();
  useDocumentTitle(`Permissions - ${currentAction.name} - Actions | Hasura`);
  const [state, setState] = useState<ActionPermissionState>('none');
  const [currentRole, setCurrentRole] = useState<string>('');
  const [newRole, setNewRole] = useState<string>('');

  const allRoles = MetadataSelectors.getRoles(metadata);
  const allPermissions = currentAction.permissions;

  const buildRow = (
    roleName: string,
    isNewRole: boolean,
  ): PermissionsTableRow => {
    const isEditable = roleName !== 'admin' && !readOnlyMode;

    const onClick = () => {
      if (state !== 'none') {
        return setState('none');
      }

      if (isNewRole && roleName) {
        setState('new_role');
        return;
      }

      if (roleName) {
        setCurrentRole(roleName);

        const existingPerm = allPermissions?.find((p) => p.role === roleName);
        if (existingPerm) {
          return setState('modify');
        }

        return setState('new_permission');
      }
    };

    const access = (() => {
      if (roleName === 'admin') return 'fullAccess';
      if (isNewRole) return 'noAccess';
      const existingPerm = allPermissions?.find((p) => p.role === roleName);
      return existingPerm ? 'fullAccess' : 'noAccess';
    })();

    return {
      roleName,
      roleCell: isNewRole
        ? {
            isNewRole: true,
            newRoleValue: newRole,
            onNewRoleValueChange: (newRoleName) => setNewRole(newRoleName),
          }
        : undefined,
      cells: {
        [PERMISSION_COLUMN.key]: {
          access,
          isEditable,
          isCurrentEdit: state !== 'none',
          onClick: isEditable ? onClick : undefined,
          'aria-label': `${roleName}-${PERMISSION_COLUMN.key}`,
          testId: `${roleName}-Permission`,
        },
      },
    };
  };

  const rows: PermissionsTableRow[] = [
    ...['admin', ...allRoles].map((role) => buildRow(role, false)),
    buildRow(newRole, true),
  ];

  return (
    <Analytics name="ActionPermissions" {...REDACT_EVERYTHING}>
      <Flex direction="column" gap="4">
        <PermissionsLegend />
        <PermissionsTableView
          columns={[PERMISSION_COLUMN]}
          rows={rows}
          roleColumnLabel="Role"
        />
        <div>
          {!readOnlyMode && state !== 'none' && (
            <PermissionEditor
              currentAction={currentAction}
              state={state}
              role={state === 'new_role' ? newRole : currentRole}
              onClose={() => setState('none')}
            />
          )}
        </div>
      </Flex>
    </Analytics>
  );
};

export default Permissions;
