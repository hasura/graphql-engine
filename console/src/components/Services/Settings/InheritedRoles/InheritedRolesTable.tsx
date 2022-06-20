import React from 'react';
import InheritedRolesTableHeader from './TableHeader';
import InheritedRolesTableBody from './TableBody';
import { InheritedRole } from '../../../../metadata/types';

export type InheritedRolesTableProps = {
  inheritedRoles: InheritedRole[];
};

const InheritedRolesTable: React.FC<InheritedRolesTableProps> = ({
  inheritedRoles,
}) => {
  const headings = ['Inherited Role', 'Role Set', 'Actions'];

  return (
    <div className="mt-md mr-md">
      <table className="flex flex-col">
        <InheritedRolesTableHeader headings={headings} />
        <InheritedRolesTableBody inheritedRoles={inheritedRoles} />
      </table>
    </div>
  );
};

export default InheritedRolesTable;
