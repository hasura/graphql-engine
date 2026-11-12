import { Badge, IconButton, Text } from '@hasura/shared/ui';
import React from 'react';
import { FaEdit, FaKey, FaTrash } from 'react-icons/fa';
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
    <tr>
      <td className="align-top py-2">
        <Flex gap="1" align="start">
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
        </Flex>
      </td>
      <td className="pl-2 pr-4 min-w-50 py-2">
        <Text weight="bold" as="div">
          {column.name}
          {column.config?.custom_name && (
            <>
              <span className="mx-2">→</span>
              <span>{column.config.custom_name}</span>
            </>
          )}
        </Text>
        {!!column.config?.comment && (
          <Text className="italic" size="1" as="div">
            {column.config?.comment}
          </Text>
        )}
      </td>
      <td className="align-top py-2">
        <Flex className="h-full" align="start" gap="2">
          <Badge color="gray" data-testid={`${column.name}-ui-data-type`}>
            {columnDataType(column.dataType || 'Unknown')}
          </Badge>

          {column.nullable && <Badge color="yellow">nullable</Badge>}

          {column.isPrimaryKey && (
            <Badge color="indigo">
              <FaKey className="mr-2 h-3" /> Primary Key
            </Badge>
          )}
        </Flex>
      </td>
    </tr>
  );
};
