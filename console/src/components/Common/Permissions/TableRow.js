import React from 'react';
import styles from './PermissionStyles.scss';

const TableRow = ({
  roleName,
  permTypes,
  bulkSection,
  isNewRole,
  dispatchRoleNameChange,
}) => {
  const rowCells = [];

  if (isNewRole) {
    rowCells.push(
      <th key={'role-textbox'}>
        <input
          id="new-role-input"
          className={`form-control ${styles.newRoleInput}`}
          onChange={dispatchRoleNameChange}
          type="text"
          placeholder="Enter new role"
          value={roleName}
          data-test="role-textbox"
        />
      </th>
    );
  } else {
    rowCells.push(
      <th data-test={`role-${roleName}`} key={'role-textbox'}>
        {roleName}
      </th>
    );
  }

  permTypes.forEach(p => {
    rowCells.push(
      <td
        key={p.dataTest}
        className={p.className}
        onClick={p.onClick}
        title="Edit permissions"
        data-test={p.dataTest}
      >
        {p.access}
        {p.editIcon}
        {p.tooltip}
      </td>
    );
  });

  if (bulkSection) {
    if (bulkSection.showCheckbox) {
      rowCells.push(
        <td key={-1} className={styles.bulkSelectCell}>
          <input
            onChange={bulkSection.onChange}
            className={`${styles.cursorPointer} legacy-input-fix`}
            checked={bulkSection.checked}
            data-role={bulkSection.role}
            disabled={bulkSection.disableCheckbox}
            title={bulkSection.title}
            type="checkbox"
          />
          {/*{deleteIcon}*/}
        </td>
      );
    } else {
      rowCells.push(
        <td key="empty-bulk-select" className={styles.bulkSelectCell} />
      );
    }
  }

  return <tr>{rowCells}</tr>;
};

export default TableRow;
