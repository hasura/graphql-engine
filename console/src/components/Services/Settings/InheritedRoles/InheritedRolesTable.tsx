import React from 'react';
import InheritedRolesTableHeader from './TableHeader';
import InheritedRolesTableBody from './TableBody';
import { InheritedRole } from '../../../../metadata/types';
import styles from './InheritedRolesStyles.scss';

export type InheritedRolesTableProps = {
  inheritedRoles: InheritedRole[];
};

const InheritedRolesTable: React.FC<InheritedRolesTableProps> = ({
  inheritedRoles,
}) => {
  const headings = ['Inherited Role', 'Role Set', 'Actions'];

  return (
    <div className={styles.margin_top}>
      <table className="table table-bordered">
        <InheritedRolesTableHeader headings={headings} />
        <InheritedRolesTableBody inheritedRoles={inheritedRoles} />
      </table>
    </div>
  );
};

export default InheritedRolesTable;
