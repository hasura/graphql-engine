import React, { ChangeEvent, useContext, useState } from 'react';
import Button from '../../../Common/Button';
import { ActionContext } from './InheritedRoles';
import { InheritedRole } from '../../../../metadata/types';
import styles from './InheritedRolesStyles.scss';

type TableRowProps = {
  inheritedRole?: InheritedRole;
};

const TableRow: React.FC<TableRowProps> = ({ inheritedRole }) => {
  const [roleName, setRoleName] = useState<string>('');
  const rowCells = [];

  const context = useContext(ActionContext);
  const onRoleNameChange = (e: ChangeEvent<HTMLInputElement>) => {
    setRoleName(e.target.value?.trim());
    context?.onRoleNameChange(e.target.value?.trim());
  };

  if (inheritedRole) {
    rowCells.push(<th key="role-textbox">{inheritedRole.role_name}</th>);
    rowCells.push(<td key="role-set">{inheritedRole.role_set.join(', ')}</td>);
    rowCells.push(
      <td key="actions">
        <Button
          size="sm"
          color="white"
          className={styles.margin_right}
          onClick={() => context?.onEdit(inheritedRole)}
        >
          Edit
        </Button>
        <Button
          size="sm"
          color="red"
          onClick={() => context?.onDelete(inheritedRole)}
        >
          Remove
        </Button>
      </td>
    );
  } else {
    rowCells.push(
      <th key="role-textbox" colSpan={3}>
        <input
          id="new-role-input"
          className={`form-control ${styles.input_box_styles}`}
          onChange={onRoleNameChange}
          type="text"
          placeholder="Enter new role"
          value={roleName}
        />
        <Button
          color="yellow"
          className={styles.create_button_styles}
          disabled={roleName.length === 0}
          onClick={() => context?.onAdd(roleName)}
        >
          Create
        </Button>
      </th>
    );
  }

  return <tr>{rowCells}</tr>;
};

export default TableRow;
