import React from 'react';
import { getRoleQueryPermissionSymbol } from './utils';
import { SelectPermission, FunctionPermission } from '@hasura/shared/types';
import {
  PermissionsTableView,
  PermissionsTableRow,
} from '../../PermissionsTable';
import { PermissionsLegend } from './PermissionsLegend';

const PERMISSION_COLUMN = { key: 'permission', label: 'Permission' };

type PermissionTableProps = {
  permCloseEdit: () => void;
  permOpenEdit: (role: string) => void;
  isEditing: boolean;
  role: string;
  allRoles: string[];
  readOnlyMode: boolean;
  allPermissions: FunctionPermission[] | undefined | null;
  isEditable: boolean;
  selectRoles: SelectPermission[] | null | undefined;
};

const PermissionsTable: React.FC<PermissionTableProps> = ({
  allPermissions,
  allRoles,
  permCloseEdit,
  permOpenEdit,
  isEditing,
  role: permEditRole,
  isEditable,
  selectRoles,
  readOnlyMode,
}) => {
  const buildRow = (role: string): PermissionsTableRow => {
    const isCurrEdit = isEditing && permEditRole === role;
    const isRowEditable = role !== 'admin' && !readOnlyMode && isEditable;

    const onClick = () => {
      if (isCurrEdit) {
        permCloseEdit();
      } else {
        permOpenEdit(role);
      }
    };

    const tooltip =
      !isEditable && role !== 'admin'
        ? 'Forbidden from edits since function permissions are inferred'
        : undefined;

    return {
      roleName: role,
      cells: {
        [PERMISSION_COLUMN.key]: {
          access: getRoleQueryPermissionSymbol(
            allPermissions,
            role,
            selectRoles,
            isEditable,
          ),
          isEditable: isRowEditable,
          isCurrentEdit: isCurrEdit,
          onClick: isRowEditable ? onClick : undefined,
          'aria-label': `${role}-${PERMISSION_COLUMN.key}`,
          testId: `${role}-Permission`,
          tooltip,
        },
      },
    };
  };

  const rows: PermissionsTableRow[] = ['admin', ...allRoles].map(buildRow);

  return (
    <>
      <PermissionsLegend />
      <PermissionsTableView
        columns={[PERMISSION_COLUMN]}
        rows={rows}
        roleColumnLabel="Role"
      />
    </>
  );
};

export default PermissionsTable;
