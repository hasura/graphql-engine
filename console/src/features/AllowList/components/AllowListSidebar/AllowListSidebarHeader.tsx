import { Button } from '@/new-components/Button';

import React from 'react';
import { FaFolderPlus } from 'react-icons/fa';
import { QueryCollectionCreateDialog } from './QueryCollectionCreateDialog';
import { AllowListStatus } from './AllowListStatus';

export const AllowListSidebarHeader = () => {
  const [isCreateModalOpen, setIsCreateModalOpen] = React.useState(false);
  return (
    <>
      {isCreateModalOpen && (
        <QueryCollectionCreateDialog
          onClose={() => setIsCreateModalOpen(false)}
        />
      )}
      <div className="flex items-center">
        <span className="text-sm font-semibold text-muted uppercase tracking-wider">
          Allow List
        </span>
        <div className="ml-1.5">
          <AllowListStatus />
        </div>
        <div className="ml-auto">
          <Button
            icon={<FaFolderPlus />}
            size="sm"
            onClick={() => setIsCreateModalOpen(true)}
          >
            Add Collection
          </Button>
        </div>
      </div>
    </>
  );
};
