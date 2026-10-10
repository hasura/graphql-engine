import { Badge, IconButton, Text } from '@hasura/shared/ui';
import React from 'react';
import { FaEdit, FaKey, FaRegComment, FaTrash } from 'react-icons/fa';
import { ModifyTableColumn } from '../types';
import { columnDataType } from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';

export const TableColumnDescription: React.FC<{
  column: ModifyTableColumn;
  onEdit: (column: ModifyTableColumn) => void;
  /** Omit when the column can't be removed (e.g. views, unsupported drivers). */
  onRemove?: (column: ModifyTableColumn) => void;
}> = ({ column, onEdit, onRemove }) => {
  return (
    <Flex gap="2" align="center" className="mb-2">
      <IconButton
        color="gray"
        variant="outline"
        title="Edit"
        size="1"
        icon={FaEdit}
        onClick={() => {
          onEdit(column);
        }}
      />
      {onRemove && (
        <IconButton
          type="button"
          color="red"
          variant="outline"
          title="Remove"
          size="1"
          aria-label={`Remove column ${column.name}`}
          onClick={() => onRemove(column)}
          icon={FaTrash}
        />
      )}

      <div>
        <Text weight="bold">
          {column.name}
          {column.config?.custom_name && (
            <>
              <span className="mx-2">→</span>
              <span className="font-normal">{column.config.custom_name}</span>
            </>
          )}
        </Text>
        {!!column.config?.comment && (
          <Text className="italic">
            <FaRegComment className="opacity-50" /> {column.config?.comment}
          </Text>
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
    </Flex>
  );
};
