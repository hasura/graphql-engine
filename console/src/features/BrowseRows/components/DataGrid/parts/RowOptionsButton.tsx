import React from 'react';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import { FaEllipsisV } from 'react-icons/fa';

export const RowOptionsButton: React.VFC<{
  row: Record<string, any>;
  onOpen: (row: Record<string, any>) => void;
}> = ({ row, onOpen }) => (
  <div className="group relative">
    <div>
      <DropdownMenu
        items={[
          [
            <div className="p-2" onClick={() => onOpen(row)}>
              Open
            </div>,
          ],
        ]}
      >
        <div className="flex items-start">
          <div className="mx-2 my-1 cursor-pointer">
            <FaEllipsisV />
          </div>
        </div>
      </DropdownMenu>
    </div>
  </div>
);
