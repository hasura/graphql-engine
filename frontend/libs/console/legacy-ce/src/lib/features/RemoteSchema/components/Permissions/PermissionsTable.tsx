import React from 'react';
import { GraphQLSchema } from 'graphql';
import {
  PermissionsTableView,
  PermissionsTableRow,
  PermissionsLegend,
} from '../../../Permissions/PermissionsTable';
import {
  buildSchemaFromRoleDefn,
  findRemoteSchemaPermission,
  getRemoteSchemaFields,
} from './utils';
import type { PermOpenEditType } from './types';
import type { AccessType, RemoteSchemaPermission } from '@hasura/shared/types';
import { PermissionEdit } from '../../hooks/useRemoteSchemaRelationshipForm';

export type PermissionsTableProps = {
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: PermOpenEditType;
  permCloseEdit: () => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
  allRoles: string[];
  currentRemoteSchema: {
    name: string;
    permissions?: RemoteSchemaPermission[];
  };
  schema: GraphQLSchema;
  bulkSelect: string[];
  readOnlyMode: boolean;
  permissionEdit: PermissionEdit;
  isEditing: boolean;
};

const PERMISSION_COLUMN = { key: 'permission', label: 'Permission' };

const PermissionsTable: React.FC<PermissionsTableProps> = ({
  allRoles,
  currentRemoteSchema,
  permissionEdit,
  isEditing,
  bulkSelect,
  readOnlyMode,
  schema,
  permSetRoleName,
  permSetBulkSelect,
  setSchemaDefinition,
  permOpenEdit,
  permCloseEdit,
}) => {
  const allPermissions = currentRemoteSchema?.permissions || [];

  const getAccess = (role: string, isNewRole: boolean): AccessType => {
    if (role === 'admin') return 'fullAccess';
    if (isNewRole) return 'noAccess';

    const existingPerm = findRemoteSchemaPermission(allPermissions, role);
    if (!existingPerm) return 'noAccess';

    const remoteFields = getRemoteSchemaFields(
      schema,
      buildSchemaFromRoleDefn(existingPerm.definition.schema),
    );
    const hasUncheckedField = remoteFields
      .filter(
        (field) =>
          !field.name.startsWith('enum') && !field.name.startsWith('scalar'),
      )
      .some((field) =>
        field.children?.some((element) => element.checked === false),
      );

    return hasUncheckedField ? 'partialAccess' : 'fullAccess';
  };

  const buildRow = (role: string, isNewRole: boolean): PermissionsTableRow => {
    const isCurrEdit =
      isEditing &&
      (permissionEdit.role === role ||
        (permissionEdit.isNewRole && permissionEdit.newRole === role));

    const isEditable = role !== 'admin' && !readOnlyMode;

    const onClick = () => {
      if (isCurrEdit) {
        permCloseEdit();
        setSchemaDefinition('');
        return;
      }

      if (isNewRole && !!role) {
        setSchemaDefinition('');
        permOpenEdit(role, isNewRole, true);
      } else if (role) {
        const existingPerm = findRemoteSchemaPermission(allPermissions, role);
        permOpenEdit(role, isNewRole, !existingPerm);
        setSchemaDefinition(existingPerm ? existingPerm.definition.schema : '');
      } else {
        document.getElementById('new-role-input')?.focus();
      }
    };

    const showBulkCheckbox = !(role === 'admin' || isNewRole);
    const disableCheckbox = !findRemoteSchemaPermission(allPermissions, role);

    return {
      roleName: role,
      roleCell: isNewRole
        ? {
            isNewRole: true,
            newRoleValue: role,
            onNewRoleValueChange: (value) => permSetRoleName(value.trim()),
          }
        : showBulkCheckbox
          ? {
              isSelectable: !disableCheckbox,
              isSelected: bulkSelect.includes(role),
              onSelectChange: () =>
                permSetBulkSelect(!bulkSelect.includes(role), role),
              disabled: disableCheckbox,
            }
          : undefined,
      cells: {
        [PERMISSION_COLUMN.key]: {
          access: getAccess(role, isNewRole),
          isEditable,
          isCurrentEdit: isCurrEdit,
          onClick: isEditable ? onClick : undefined,
          'aria-label': `${role}-${PERMISSION_COLUMN.key}`,
          testId: `${role}-Permission`,
        },
      },
    };
  };

  const rows: PermissionsTableRow[] = [
    ...['admin', ...allRoles].map((role) => buildRow(role, false)),
    buildRow(permissionEdit.newRole, true),
  ];

  return (
    <div>
      <PermissionsLegend />
      <PermissionsTableView
        columns={[PERMISSION_COLUMN]}
        rows={rows}
        roleColumnLabel="Role"
      />
    </div>
  );
};

export default PermissionsTable;
