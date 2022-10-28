import React from 'react';
import { Button } from '@/new-components/Button';

import { useBulkDeletePermissions } from './hooks';

export interface BulkDeleteProps {
  currentSource: string;
  dataSourceName: string;
  roles: string[];
  table: unknown;
  handleClose: () => void;
}

export const BulkDelete: React.FC<BulkDeleteProps> = ({
  currentSource,
  dataSourceName,
  roles,
  table,
  handleClose,
}) => {
  const { submit, isLoading, isError } = useBulkDeletePermissions({
    currentSource,
    dataSourceName,
    table,
  });

  const handleDelete = async () => {
    await submit(roles);
    handleClose();
  };

  if (isError) {
    return <div>Error bulk deleting permissions</div>;
  }

  return (
    <div className="bg-white rounded p-md border border-gray-300 grid gap-2 justify-start">
      <h2>Apply Bulk Actions</h2>
      <div className="flex gap-2">
        <p>
          <strong>Selected Roles:</strong>
        </p>{' '}
        {roles.map(role => (
          <p key={role}>{role}</p>
        ))}
      </div>
      <Button
        mode="destructive"
        type="button"
        isLoading={isLoading}
        onClick={handleDelete}
      >
        Remove All Permissions
      </Button>
    </div>
  );
};
