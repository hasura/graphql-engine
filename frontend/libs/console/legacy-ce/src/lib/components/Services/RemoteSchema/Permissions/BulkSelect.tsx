import React from 'react';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { Button } from '../../../../new-components/Button';

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
        <span key={role} className="pr-sm">
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
    <div id="bulk-section" className="bg-white border border-gray-300 p-sm">
      <div className="text-lg font-bold mb-md">Apply Bulk Actions</div>
      <div>
        <span className="pr-sm">Selected Roles</span>
        {getSelectedRoles()}
      </div>
      <div className="mt-sm mb-sm">
        <Button onClick={handleBulkRemoveClick} mode="destructive" size="sm">
          Remove All Permissions
        </Button>
      </div>
    </div>
  );
};

export default BulkSelect;
