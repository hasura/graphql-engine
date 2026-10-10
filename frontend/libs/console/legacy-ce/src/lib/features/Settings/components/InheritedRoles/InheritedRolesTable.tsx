import React, { ChangeEvent, useContext } from 'react';
import { Button, CardedTable, Input } from '@hasura/shared/ui';
import { InheritedRole } from '@hasura/shared/types';
import { ActionContext } from './InheritedRoles';
import { Flex } from '@radix-ui/themes';

export type InheritedRolesTableProps = {
  inheritedRoles: InheritedRole[];
};

const headings = ['Inherited Role', 'Role Set', 'Actions'];

const InheritedRolesTable: React.FC<InheritedRolesTableProps> = ({
  inheritedRoles,
}) => {
  const context = useContext(ActionContext);
  const onRoleNameChange = (e: ChangeEvent<HTMLInputElement>) => {
    context.onRoleNameChange(e.target.value?.trim());
  };

  return (
    <CardedTable
      columns={headings}
      data={[
        ...inheritedRoles.map((inheritedRole, i) => [
          inheritedRole.role_name,
          inheritedRole.role_set.join(', '),
          <Flex
            align="center"
            gap="2"
            key={`${inheritedRole.role_name}-actions`}
          >
            <Button
              mode="default"
              size="sm"
              className="mr-4"
              onClick={() => context?.onEdit(inheritedRole)}
            >
              Edit
            </Button>
            <Button
              size="sm"
              mode="destructive"
              onClick={() => context?.onDelete(inheritedRole)}
            >
              Remove
            </Button>
          </Flex>,
        ]),
        [
          <Flex gap="2" align="center" key="new-role-row">
            <Input
              id="new-role-input"
              containerClassName="w-full"
              onChange={onRoleNameChange}
              type="text"
              placeholder="Enter new role"
              value={context.inheritedRoleName}
            />
            <Button
              mode="primary"
              disabled={!context.inheritedRoleName}
              onClick={() => context?.onAdd(context.inheritedRoleName)}
            >
              Create
            </Button>
          </Flex>,
        ],
      ]}
    />
  );
};

export default InheritedRolesTable;
