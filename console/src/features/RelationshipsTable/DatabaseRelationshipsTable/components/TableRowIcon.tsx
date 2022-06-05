import React from 'react';
import {
  FaColumns,
  FaDatabase,
  FaFont,
  FaProjectDiagram,
  FaTable,
} from 'react-icons/fa';

const className = 'fill-current text-sm text-muted p-0';

interface TableRowIconProps {
  type:
    | 'database'
    | 'table'
    | 'remote_schema'
    | 'remote_schema_leaf'
    | 'table_leaf';
}

export const TableRowIcon = ({ type }: TableRowIconProps) => {
  switch (type) {
    case 'database':
      return <FaDatabase className={className} />;
    case 'table':
      return <FaTable className={className} />;
    case 'remote_schema':
      return <FaProjectDiagram className={className} />;
    case 'remote_schema_leaf':
      return <FaFont className={className} />;
    case 'table_leaf':
      return <FaColumns className={className} />;
    default:
      return null;
  }
};
