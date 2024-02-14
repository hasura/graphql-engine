/* eslint-disable no-underscore-dangle */
import React from 'react';
import { Button } from '../../../../new-components/Button';
import { FaFolderPlus } from 'react-icons/fa';
import { QueryCollectionCreateDialog } from './QueryCollectionCreateDialog';
import { AllowListStatus } from './AllowListStatus';

interface AllowListSidebarHeaderProps {
  onQueryCollectionCreate?: (name: string) => void;
}

export const AllowListSidebarHeader = (props: AllowListSidebarHeaderProps) => {
  const { onQueryCollectionCreate } = props;
  const [isCreateModalOpen, setIsCreateModalOpen] = React.useState(false);
  return (
    <div className="pb-4">
      {isCreateModalOpen && (
        <QueryCollectionCreateDialog
          onCreate={name => {
            if (onQueryCollectionCreate) {
              onQueryCollectionCreate(name);
            }
          }}
          onClose={() => setIsCreateModalOpen(false)}
        />
      )}
      <div className="flex flex-col 2xl:flex-row">
        <div className="flex items-center ">
          <span className="text-sm font-semibold text-muted uppercase tracking-wider whitespace-nowrap">
            Allow List
          </span>
          <div className="ml-1.5">
            <AllowListStatus />
          </div>
        </div>
        {onQueryCollectionCreate && (
          <div className="mt-2 2xl:mt-0 2xl:ml-auto">
            <Button
              icon={<FaFolderPlus />}
              size="sm"
              onClick={() => setIsCreateModalOpen(true)}
            >
              Add Collection
            </Button>
          </div>
        )}
      </div>
    </div>
  );
};
