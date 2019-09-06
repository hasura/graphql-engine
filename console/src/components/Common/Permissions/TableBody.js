import React from 'react';
import styles from './PermissionStyles.scss';
import PermTableRow from './TableRow';

const TableBody = ({
  rolePermissions,
  dispatchRoleNameChange,
}) => {

  const getTableRows = () => {
    return rolePermissions.map(rolePermission => {
      return (
        <PermTableRow
          dispatchRoleNameChange={dispatchRoleNameChange}
          {...rolePermission}
        />
      )
    })
  };

  return <tbody>{getTableRows()}</tbody>;
};

export default TableBody;
