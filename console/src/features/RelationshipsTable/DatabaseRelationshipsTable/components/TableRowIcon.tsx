import React from 'react';
import { FaColumns, FaDatabase, FaFont, FaPlug, FaTable } from 'react-icons/fa';

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
      return <FaDatabase className={className} title="Database" />;
    case 'table':
      return <FaTable className={className} title="Table" />;
    case 'remote_schema':
      return <FaPlug className={className} title="Remote Schema" />;
    case 'remote_schema_leaf':
      return <FaFont className={className} title="Field" />;
    case 'table_leaf':
      return <FaColumns className={className} title="Column" />;
    default:
      return null;
  }
};
