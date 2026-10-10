import React from 'react';
import { getConfirmation } from '@hasura/shared/utils';
import { Button } from '@hasura/shared/ui';

export type BulkSelectProps = {
  bulkSelect: string[];
  permRemoveMultipleRoles: () => void;
};

const BulkSelect: React.FC<BulkSelectProps> = ({
  bulkSelect,
  permRemoveMultipleRoles,
}) => {
  const getSelectedRoles = () => {
    return bulkSelect.map((role: string) => {
      return (
        <span key={role} className="pr-2">
          <b>{role}</b>{' '}
        </span>
      );
    });
  };

  const handleBulkRemoveClick = () => {
    const confirmMessage =
      'This will remove all currently set permissions for the selected role(s)';
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      permRemoveMultipleRoles();
    }
  };

  return (
    <div id="bulk-section" className="bg-white border border-gray-300 p-2">
      <div className="text-lg font-bold mb-4">Apply Bulk Actions</div>
      <div>
        <span className="pr-2">Selected Roles</span>
        {getSelectedRoles()}
      </div>
      <div className="mt-2 mb-2">
        <Button onClick={handleBulkRemoveClick} mode="destructive" size="sm">
          Remove All Permissions
        </Button>
      </div>
    </div>
  );
};

export default BulkSelect;
