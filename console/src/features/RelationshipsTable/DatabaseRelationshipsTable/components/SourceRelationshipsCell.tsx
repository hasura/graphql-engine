import React from 'react';

import { TableRowIcon } from './TableRowIcon';
import { RowData } from '../types';

interface RelationshipCellProps {
  row: RowData;
}

export const SourceRelationshipCell = ({ row }: RelationshipCellProps) => {
  // these will be updated when the table also handles other db to x relationships
  const secondaryIconFromType = 'table_leaf';

  return (
    <div className="flex items-center">
      <div className="flex gap-2 items-center">
        <TableRowIcon type={row.fromType} />
        {row?.source}
        {row?.fieldsFrom.map(field => (
          <React.Fragment key={field}>
            &nbsp;/
            <TableRowIcon type={secondaryIconFromType} />
            {field}
          </React.Fragment>
        ))}
      </div>
    </div>
  );
};
