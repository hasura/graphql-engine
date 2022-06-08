import React from 'react';
import { TableRowIcon } from './TableRowIcon';
import { RowData } from '../types';

interface RelationshipCellProps {
  row: RowData;
}

export const DestinationRelationshipCell = ({ row }: RelationshipCellProps) => {
  // these will be updated when the table also handles other db to x relationships
  const secondaryIconToType =
    row.toType === 'remote_schema' ? 'remote_schema_leaf' : 'table_leaf';

  return (
    <div className="flex items-center">
      <div className="flex gap-2 items-center">
        <TableRowIcon
          type={row.toType === 'remote_schema' ? 'remote_schema' : 'table'}
        />
        {row.toType === 'remote_schema' ? row?.target : row?.targetTable}
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
