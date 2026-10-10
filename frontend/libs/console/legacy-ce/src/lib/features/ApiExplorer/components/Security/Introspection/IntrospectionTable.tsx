import React, { useState } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Flex, Text } from '@radix-ui/themes';
import { Button, Input, Table } from '@hasura/shared/ui';
import IntrospectionForm from './IntrospectionForm';
import SecurityLegends, { Legends } from '../SecurityLegends';

type IntrospectionRow = { roleName: string; introspectionIsDisabled: boolean };

type Props = {
  rows: IntrospectionRow[];
};

const IntrospectionTable: React.FC<Props> = ({ rows }) => {
  const [newRole, setNewRole] = useState('');
  const [editingRole, setEditingRole] = useState<string | null>(null);

  const closeForm = () => {
    setEditingRole(null);
    setNewRole('');
  };

  const addNewRole = () => {
    const role = newRole.trim();
    if (role) setEditingRole(role);
  };

  const rowProps = (role: string) => ({
    className: 'cursor-pointer hover:bg-[var(--gray-a3)]',
    tabIndex: 0,
    'aria-selected': editingRole === role,
    onClick: () => setEditingRole(role),
    onKeyDown: (e: React.KeyboardEvent) => {
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault();
        setEditingRole(role);
      }
    },
  });

  // Roles that aren't listed in `disabled_for_roles` have introspection enabled.
  const isDisabledForRole = (role: string) =>
    rows.find((row) => row.roleName === role)?.introspectionIsDisabled ?? false;

  return (
    <Analytics
      name="SecuritySettingsSchemaIntrospection"
      {...REDACT_EVERYTHING}
    >
      <Flex direction="column" gap="4">
        <SecurityLegends />
        <Table.Root variant="surface">
          <Table.Header>
            <Table.Row>
              <Table.ColumnHeaderCell>Role</Table.ColumnHeaderCell>
              <Table.ColumnHeaderCell align="center">
                Schema Introspection
              </Table.ColumnHeaderCell>
            </Table.Row>
          </Table.Header>
          <Table.Body>
            <Table.Row>
              <Table.RowHeaderCell>admin</Table.RowHeaderCell>
              <Table.Cell align="center">
                <Text color="gray">full access</Text>
              </Table.Cell>
            </Table.Row>
            {rows.map(({ roleName, introspectionIsDisabled }) => (
              <Table.Row key={roleName} {...rowProps(roleName)}>
                <Table.RowHeaderCell>{roleName}</Table.RowHeaderCell>
                <Table.Cell>
                  <Flex justify="center">
                    {introspectionIsDisabled ? (
                      <Legends.Disabled />
                    ) : (
                      <Legends.Enabled />
                    )}
                  </Flex>
                </Table.Cell>
              </Table.Row>
            ))}
            <Table.Row>
              <Table.RowHeaderCell>
                <Flex gap="2" align="center">
                  <Input
                    value={newRole}
                    onChange={(e) => setNewRole(e.target.value)}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter') addNewRole();
                    }}
                    placeholder="Enter new role"
                  />
                  <Button
                    size="sm"
                    mode="default"
                    disabled={!newRole.trim()}
                    onClick={addNewRole}
                  >
                    Configure
                  </Button>
                </Flex>
              </Table.RowHeaderCell>
              <Table.Cell>
                <Flex justify="center">
                  <Legends.Enabled />
                </Flex>
              </Table.Cell>
            </Table.Row>
          </Table.Body>
        </Table.Root>
      </Flex>
      {editingRole !== null && (
        <IntrospectionForm
          role={editingRole}
          introspectionIsDisabled={isDisabledForRole(editingRole)}
          onClose={closeForm}
        />
      )}
    </Analytics>
  );
};

export default IntrospectionTable;
