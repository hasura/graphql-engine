import React from 'react';
import { FaArrowRight } from 'react-icons/fa';

import { TableRowIcon } from './TableRowIcon';
import { RowData } from '../types';

interface RelationshipCellProps {
  row: RowData;
}

export const RelationshipCell = ({ row }: RelationshipCellProps) => {
  // these will be updated when the table also handles other db to x relationships
  const secondaryIconFromType = 'table_leaf';
  const secondaryIconToType =
    row.toType === 'remote_schema' ? 'remote_schema_leaf' : 'table_leaf';

  return (
    <div className="flex items-center gap-4">
      <div className="flex gap-2 items-center">
        <TableRowIcon type={row.fromType} />
        {row?.reference}
        {row?.fieldsFrom.map(field => (
          <React.Fragment key={field}>
            &nbsp;/
            <TableRowIcon type={secondaryIconFromType} />
            {field}
          </React.Fragment>
        ))}
      </div>
      <FaArrowRight className="fill-current text-sm text-muted col-span-1" />
      <div className="flex gap-2 items-center">
        <TableRowIcon type={row.toType} />
        {row?.target}
        {row?.fieldsTo.map(field => (
          <React.Fragment key={field}>
            &nbsp;/
            <TableRowIcon type={secondaryIconToType} />
            {field}
          </React.Fragment>
        ))}
      </div>
    </div>
  );
};
