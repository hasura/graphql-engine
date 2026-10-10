import React from 'react';
import { DropdownMenu, IconButton } from '@hasura/shared/ui';
import { RiMore2Fill } from 'react-icons/ri';

export const RowOptionsButton: React.FC<{
  row: Record<string, any>;
  onOpen: (row: Record<string, any>) => void;
  onClone: (row: Record<string, any>) => void;
  onEdit: (row: Record<string, any>) => void;
  onDelete: (row: Record<string, any>) => void;
}> = ({ row, onOpen, onClone, onEdit, onDelete }) => (
  <div className="group relative">
    <div>
      <DropdownMenu.Root
        items={[
          <DropdownMenu.Item key="open" onSelect={() => onOpen(row)}>
            Open
          </DropdownMenu.Item>,
          <DropdownMenu.Item key="edit" onSelect={() => onEdit(row)}>
            Edit row
          </DropdownMenu.Item>,
          <DropdownMenu.Item key="clone" onSelect={() => onClone(row)}>
            Clone row
          </DropdownMenu.Item>,
          <DropdownMenu.Item
            key="delete"
            color="red"
            onSelect={() => onDelete(row)}
          >
            Delete
          </DropdownMenu.Item>,
        ]}
      >
        <IconButton mode="default" variant="ghost" radius="full">
          <RiMore2Fill />
        </IconButton>
      </DropdownMenu.Root>
    </div>
  </div>
);
