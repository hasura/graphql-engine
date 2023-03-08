import React from 'react';
import { DropdownMenu } from '../../../../../new-components/DropdownMenu';
import { RiMore2Fill } from 'react-icons/ri';

export const RowOptionsButton: React.VFC<{
  row: Record<string, any>;
  onOpen: (row: Record<string, any>) => void;
  onDelete: (row: Record<string, any>) => void;
}> = ({ row, onOpen, onDelete }) => (
  <div className="group relative">
    <div>
      <DropdownMenu
        items={[
          [
            <div className="p-2" onClick={() => onOpen(row)}>
              Open
            </div>,
            <div className="p-2" onClick={() => onDelete(row)}>
              Delete
            </div>,
          ],
        ]}
      >
        <div className="flex items-start">
          <div className="mx-2 my-1 cursor-pointer group-hover:opacity-100">
            <RiMore2Fill size="14px" />
          </div>
        </div>
      </DropdownMenu>
    </div>
  </div>
);
