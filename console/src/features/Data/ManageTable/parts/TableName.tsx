import { Badge } from '@/new-components/Badge';
import { Button } from '@/new-components/Button';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import React from 'react';
import { FaChevronDown, FaDatabase } from 'react-icons/fa';

export const TableName: React.VFC<{
  dataSourceName: string;
  tableName: string;
}> = ({ tableName }) => (
  <div className="flex items-center gap-3 mb-3">
    <div className="group relative">
      <div>
        <DropdownMenu
          items={[
            [
              // TODO: To be implemented after metadata util functions have been added to the metadata library
              <span className="py-xs text-red-600" onClick={() => {}}>
                Untrack {tableName}
              </span>,
            ],
          ]}
        >
          <div className="flex gap-0.5 items-center">
            <Button
              iconPosition="end"
              icon={
                <FaChevronDown
                  size={12}
                  className="text-gray-400 text-sm transition-transform group-radix-state-open:rotate-180"
                />
              }
            >
              <div className="flex flex-row items-center ">
                <FaDatabase className="mr-1.5" size={12} />
                <span className="text-lg">{tableName}</span>
              </div>
            </Button>
          </div>
        </DropdownMenu>
      </div>
    </div>
    <div>
      <Badge color="green">Tracked</Badge>
    </div>
  </div>
);
