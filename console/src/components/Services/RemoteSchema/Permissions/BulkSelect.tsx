import React from 'react';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

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
        <span key={role} className={styles.add_pad_right}>
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
    <div id="bulk-section" className={styles.activeEdit}>
      <div className={styles.editPermsHeading}>Apply Bulk Actions</div>
      <div>
        <span className={styles.add_pad_right}>Selected Roles</span>
        {getSelectedRoles()}
      </div>
      <div className={`${styles.add_mar_top} ${styles.add_mar_bottom_mid}`}>
        <Button onClick={handleBulkRemoveClick} color="red" size="sm">
          Remove All Permissions
        </Button>
      </div>
    </div>
  );
};

export default BulkSelect;
