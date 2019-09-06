import React from 'react';
import PermTableRow from './TableRow';

const TableBody = ({ rolePermissions, dispatchRoleNameChange }) => {
  const getTableRows = () => {
    return rolePermissions.map((rolePermission, i) => {
      return (
        <PermTableRow
          key={i}
          dispatchRoleNameChange={dispatchRoleNameChange}
          {...rolePermission}
        />
      );
    });
  };

  return <tbody>{getTableRows()}</tbody>;
};

export default TableBody;
