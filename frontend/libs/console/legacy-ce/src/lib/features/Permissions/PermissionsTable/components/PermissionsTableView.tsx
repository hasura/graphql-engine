import React from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import { Checkbox, Input, Table } from '@hasura/shared/ui';
import { AccessType } from '@hasura/shared/types';
import { PermissionAccessCell } from './Cells';

export interface PermissionsTableColumn {
  key: string;
  label: string;
}

export interface PermissionsTableCellData {
  access: AccessType;
  isEditable: boolean;
  isCurrentEdit: boolean;
  onClick?: () => void;
  'aria-label'?: string;
  testId?: string;
  tooltip?: React.ReactNode;
}

export interface PermissionsTableRoleCell {
  /** Renders a text input for entering a new role name instead of a static label. */
  isNewRole?: boolean;
  newRoleValue?: string;
  onNewRoleValueChange?: (value: string) => void;
  /** Renders a checkbox next to the role name (e.g. for bulk selection). */
  isSelectable?: boolean;
  isSelected?: boolean;
  onSelectChange?: () => void;
  disabled?: boolean;
}

export interface PermissionsTableRow {
  roleName: string;
  roleCell?: PermissionsTableRoleCell;
  cells: Record<string, PermissionsTableCellData>;
}

export interface PermissionsTableViewProps {
  columns: PermissionsTableColumn[];
  rows: PermissionsTableRow[];
  roleColumnLabel?: string;
}

const RoleCell: React.FC<{
  roleName: string;
  roleCell?: PermissionsTableRoleCell;
}> = ({ roleName, roleCell }) => {
  if (roleCell?.isNewRole) {
    return (
      <Table.ColumnHeaderCell align="center">
        <Input
          id="new-role-input"
          data-testid="new-role-input"
          value={roleCell.newRoleValue}
          aria-label="create-new-role"
          placeholder="Create new role..."
          onChange={(e) => roleCell.onNewRoleValueChange?.(e.target.value)}
        />
      </Table.ColumnHeaderCell>
    );
  }

  if (roleCell?.isSelectable !== undefined) {
    return (
      <Table.ColumnHeaderCell align="center">
        <Flex align="center">
          <Checkbox
            id={roleName}
            value={roleCell.isSelected}
            onChange={roleCell.onSelectChange}
            disabled={!roleCell.isSelectable || !!roleCell.disabled}
          >
            <Strong>{roleName}</Strong>
          </Checkbox>
        </Flex>
      </Table.ColumnHeaderCell>
    );
  }

  return (
    <Table.RowHeaderCell>
      <Strong>{roleName}</Strong>
    </Table.RowHeaderCell>
  );
};

/**
 * Presentational-only permissions table: renders a role column plus one
 * column per supported action, and is agnostic of the caller's state
 * management. Used by Data, Actions, and Remote Schema permissions tabs.
 */
export const PermissionsTableView: React.FC<PermissionsTableViewProps> = ({
  columns,
  rows,
  roleColumnLabel = 'ROLE',
}) => (
  <Table.Root variant="surface">
    <Table.Header>
      <Table.Row>
        <Table.RowHeaderCell>
          <Strong>{roleColumnLabel}</Strong>
        </Table.RowHeaderCell>
        {columns.map((column) => (
          <Table.RowHeaderCell key={column.key} align="center">
            <Strong>{column.label}</Strong>
          </Table.RowHeaderCell>
        ))}
      </Table.Row>
    </Table.Header>

    <Table.Body>
      {rows.map((row) => (
        <Table.Row key={row.roleCell?.isNewRole ? 'new-role' : row.roleName}>
          <RoleCell roleName={row.roleName} roleCell={row.roleCell} />
          {columns.map((column) => {
            const cell = row.cells[column.key];
            if (!cell) return <Table.Cell key={column.key} />;
            return <PermissionAccessCell key={column.key} {...cell} />;
          })}
        </Table.Row>
      ))}
    </Table.Body>
  </Table.Root>
);
