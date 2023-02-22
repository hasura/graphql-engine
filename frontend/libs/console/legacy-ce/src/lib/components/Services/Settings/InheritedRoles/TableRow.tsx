import React, { ChangeEvent, useContext, useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { ActionContext } from './InheritedRoles';
import { InheritedRole } from '../../../../metadata/types';
import { inputStyles } from '../constants';

type TableRowProps = {
  inheritedRole?: InheritedRole;
};

const TableRow: React.FC<TableRowProps> = ({ inheritedRole }) => {
  const [roleName, setRoleName] = useState<string>('');

  const context = useContext(ActionContext);
  const onRoleNameChange = (e: ChangeEvent<HTMLInputElement>) => {
    setRoleName(e.target.value?.trim());
    context?.onRoleNameChange(e.target.value?.trim());
  };

  if (inheritedRole) {
    return (
      <div className="flex border border-t-0">
        <div className="w-full p-xs font-bold bg-[#f0f0f0]" key="role-textbox">
          {inheritedRole.role_name}
        </div>
        <div className="w-full p-xs border-l" key="role-set">
          {inheritedRole.role_set.join(', ')}
        </div>
        <div className="w-full border-l p-xs -left-1" key="actions">
          <Button
            size="sm"
            className="mr-md"
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
        </div>
      </div>
    );
  }
  return (
    <div className="p-xs flex bg-[#f0f0f0] w-full border" key="role-textbox">
      <input
        id="new-role-input"
        className={`${inputStyles} w-56 mr-xs bg-white`}
        onChange={onRoleNameChange}
        type="text"
        placeholder="Enter new role"
        value={roleName}
      />
      <Button
        mode="primary"
        disabled={roleName.length === 0}
        onClick={() => context?.onAdd(roleName)}
      >
        Create
      </Button>
    </div>
  );
};

export default TableRow;
