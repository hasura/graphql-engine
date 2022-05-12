import React from 'react';
import { TableRowIcon } from './TableRowIcon';
import { RowData } from '../types';

interface RelationshipCellProps {
  row: RowData;
}

export const DestinationRelationshipCell = ({ row }: RelationshipCellProps) => {
  // these will be updated when the table also handles other db to x relationships
  const secondaryIconToType =
    row.type === 'remoteSchema' ? 'remote_schema_leaf' : 'table_leaf';

  return (
    <div className="flex items-center">
      <div className="flex gap-2 items-center">
        <TableRowIcon type={row.toType} />
        {row?.destination}
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
