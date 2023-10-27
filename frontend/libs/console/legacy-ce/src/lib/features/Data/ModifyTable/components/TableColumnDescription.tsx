import { Badge } from '../../../../new-components/Badge';
import { Button } from '../../../../new-components/Button';
import React from 'react';
import { FaKey, FaRegComment } from 'react-icons/fa';
import { ModifyTableColumn } from '../types';
import { columnDataType } from '../../../DataSource/utils';

export const TableColumnDescription: React.VFC<{
  column: ModifyTableColumn;
  onEdit: (column: ModifyTableColumn) => void;
}> = ({ column, onEdit }) => {
  return (
    <div key={column.name} className="flex gap-4 items-center mb-2">
      {/* To assist with tests */}
      <input
        type="hidden"
        data-data-type={columnDataType(column.dataType)}
        data-console-data-type={column.consoleDataType}
        data-testid={`${column.name}-data`}
      />
      <Button
        size="sm"
        onClick={() => {
          onEdit(column);
        }}
      >
        Edit
      </Button>

      <div>
        <div className="font-bold">
          {column.name}
          {column.config?.custom_name && (
            <>
              <span className="mx-2">→</span>
              <span className="font-normal">{column.config.custom_name}</span>
            </>
          )}
        </div>
        {!!column.config?.comment && (
          <div className="italic">
            <FaRegComment className="opacity-50" /> {column.config?.comment}
          </div>
        )}
      </div>
      <div>
        <Badge color="gray">
          <span data-testid={`${column.name}-ui-data-type`}>
            {columnDataType(column.dataType || 'Unknown')}
          </span>
        </Badge>
      </div>

      {column.nullable && <Badge color="yellow">nullable</Badge>}

      {column.isPrimaryKey && (
        <Badge color="indigo">
          <FaKey className="mr-2 h-3" /> Primary Key
        </Badge>
      )}
    </div>
  );
};
