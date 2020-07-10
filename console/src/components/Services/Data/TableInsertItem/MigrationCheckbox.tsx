import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';

type MigrationCheckBoxProps = {
  isChecked: boolean;
  onChange: () => void;
};

const MigrationCheckBox: React.FC<MigrationCheckBoxProps> = ({
  isChecked,
  onChange,
}) => (
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
      message="create the current insertion as a migration"
    />
  </label>
);

export default MigrationCheckBox;
