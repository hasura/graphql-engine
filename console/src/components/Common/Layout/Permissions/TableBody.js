import React from 'react';
import styles from './PermissionStyles.scss';

const TableBody = ({
  rolePermissions,
  dispatchRoleNameChange,
  getPermTypes,
  supportBulk
}) => {
  const getTableRow = (rolePermission, isNewRole = false) => {
    const rowCells = [];

    // bulk select checkbox
    if (supportBulk) {
      if (rolePermission.name === 'admin' || isNewRole) {
        rowCells.push(<td key="empty-admin-cell" />);
      } else {
        rowCells.push(
          <td
            key={-1}
          >
            <div>
              <input
                onChange={rolePermission.dispatchBulkSelect}
                checked={rolePermission.bulkCheck}
                data-role={rolePermission.name}
                title="Select for bulk actions"
                type="checkbox"
              />
              {/*{deleteIcon}*/}
            </div>
          </td>
        );
      }
    }

    // role name or role textbox
    if (isNewRole) {
      rowCells.push(
        <td
          key={'role-textbox'}
        >
          <input
            id="newRoleInput"
            className={`form-control ${styles.newRoleInput}`}
            onChange={dispatchRoleNameChange}
            type="text"
            placeholder="Enter new role"
            defaultValue={''}
            data-test="role-textbox"
          />
        </td>
      );
    } else {
      rowCells.push(<td key={'role-textbox'}>{rolePermission.name}</td>);
    }

    // appropriate icons showing the allowed levels for each type
    const permTypes = getPermTypes(rolePermission.name, isNewRole);
    permTypes.forEach((permType, i) => {
      rowCells.push(
        <td
          key={i}
          className={permType.className}
          onClick={permType.onClick}
          title="Edit permissions"
          data-test={`${rolePermission.name || ''}-${permType.name}`}
        >
          {permType.permSymbol}
          {permType.editLink}
        </td>
      );
    });

    return rowCells;
  };

  // wrapper for rows
  const getTableRows = () => {
    const tableRows = rolePermissions.map(rolePermission => (
      <tr>{getTableRow(rolePermission)}</tr>
    ));
    tableRows.push(<tr>{getTableRow({}, true)}</tr>);
    return tableRows;
  };

  return <tbody>{getTableRows()}</tbody>;
};

export default TableBody;

// bulkSelect.filter(e => e === rolePermission.name).length
