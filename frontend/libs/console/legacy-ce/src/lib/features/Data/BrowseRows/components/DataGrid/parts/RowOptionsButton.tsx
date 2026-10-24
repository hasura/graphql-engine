import React from 'react';
import { DropdownMenu, IconButton } from '@hasura/shared/ui';
import { RiMore2Fill } from 'react-icons/ri';

export const RowOptionsButton: React.FC<{
  row: Record<string, any>;
  onOpen: (row: Record<string, any>) => void;
  onClone: (row: Record<string, any>) => void;
  /** Omit when the row can't be identified, e.g. a table without a primary key. */
  onEdit?: (row: Record<string, any>) => void;
  /** Omit when the row can't be identified, e.g. a table without a primary key. */
  onDelete?: (row: Record<string, any>) => void;
}> = ({ row, onOpen, onClone, onEdit, onDelete }) => (
  <div className="group relative">
    <div>
      <DropdownMenu.Root
        items={[
          <DropdownMenu.Item key="open" onSelect={() => onOpen(row)}>
            Open
          </DropdownMenu.Item>,
          onEdit && (
            <DropdownMenu.Item key="edit" onSelect={() => onEdit(row)}>
              Edit row
            </DropdownMenu.Item>
          ),
          <DropdownMenu.Item key="clone" onSelect={() => onClone(row)}>
            Clone row
          </DropdownMenu.Item>,
          onDelete && (
            <DropdownMenu.Item
              key="delete"
              color="red"
              onSelect={() => onDelete(row)}
            >
              Delete
            </DropdownMenu.Item>
          ),
        ].filter((item): item is React.ReactElement => Boolean(item))}
      >
        <IconButton mode="default" variant="ghost" radius="full">
          <RiMore2Fill />
        </IconButton>
      </DropdownMenu.Root>
    </div>
  </div>
);
