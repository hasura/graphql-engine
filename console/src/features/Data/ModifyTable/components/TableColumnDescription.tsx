import { TableColumn } from '@/features/DataSource';
import { Badge } from '@/new-components/Badge';
import { Button } from '@/new-components/Button';
import { useFireNotification } from '@/new-components/Notifications';
import React from 'react';
import { FaKey } from 'react-icons/fa';

export const TableColumnDescription: React.VFC<{
  column: TableColumn;
}> = ({ column }) => {
  const { fireNotification } = useFireNotification();
  return (
    <div key={column.name} className="flex gap-4 items-center mb-2">
      <Button
        size="sm"
        // disabled
        onClick={() => {
          fireNotification({
            message: 'This feature is coming soon.',
            title: 'Feature Not Available',
            type: 'info',
          });
        }}
      >
        Edit
      </Button>

      <strong>{column.name}</strong>

      <div>{column.dataType}</div>

      {column.nullable && <Badge color="gray">nullable</Badge>}

      {column.isPrimaryKey && (
        <Badge color="indigo">
          <FaKey className="mr-2 h-3" /> Primary Key
        </Badge>
      )}
    </div>
  );
};
