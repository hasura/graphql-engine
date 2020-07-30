import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';

type MigrationCheckboxProps = {
  isChecked: boolean;
  onChange: () => void;
  className: string;
};

const MigrationCheckbox: React.FC<MigrationCheckboxProps> = ({
  isChecked,
  onChange,
  className,
}) => (
  <div className={className}>
    <label className={styles.labelText}>
      <input
        type="checkbox"
        checked={isChecked}
        title="This is a migration"
        onChange={onChange}
        className={styles.migrationCheckbox}
      />
      This is a migration
      <ToolTip
        placement="right"
        message="Create a migration file with the current insertion"
      />
    </label>
  </div>
);

export default MigrationCheckbox;
