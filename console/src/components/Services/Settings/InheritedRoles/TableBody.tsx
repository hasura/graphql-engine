import React from 'react';
import { InheritedRole } from '../../../../metadata/types';
import TableRow from './TableRow';

type TableBodyProps = {
  inheritedRoles: InheritedRole[];
};

const TableBody: React.FC<TableBodyProps> = props => {
  const { inheritedRoles } = props;

  return (
    <tbody>
      {inheritedRoles.map((inheritedRole, i) => (
        <TableRow key={i} inheritedRole={inheritedRole} />
      ))}
      <TableRow key={inheritedRoles.length} />
    </tbody>
  );
};

export default TableBody;
