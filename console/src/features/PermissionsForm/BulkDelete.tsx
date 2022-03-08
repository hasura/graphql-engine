import React from 'react';
import { Button } from '@/new-components/Button';

import { useBulkDeletePermissions } from './hooks';

export interface BulkDeleteProps {
  roles: string[];
  tableName: string;
  schemaName: string;
  handleClose: () => void;
}

export const BulkDelete: React.FC<BulkDeleteProps> = ({
  roles,
  tableName,
  schemaName,
  handleClose,
}) => {
  const { submit, isLoading, isError } = useBulkDeletePermissions({
    tableName,
    schemaName,
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
