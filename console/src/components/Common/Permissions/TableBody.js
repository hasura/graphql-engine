import React from 'react';
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
      );
    });
  };

  return <tbody>{getTableRows()}</tbody>;
};

export default TableBody;
