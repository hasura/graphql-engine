import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';

type MigrationCheckBoxProps = {
  isChecked: boolean;
  onChange: () => void;
  className: string;
};

const MigrationCheckBox: React.FC<MigrationCheckBoxProps> = ({
  isChecked,
  onChange,
  className,
}) => (
  <label className={`${styles.labelText} ${className}`}>
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
);

export default MigrationCheckBox;
