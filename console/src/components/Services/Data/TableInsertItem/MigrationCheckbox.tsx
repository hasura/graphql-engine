import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';

type MigrationCheckboxProps = {
  isChecked: boolean;
  onChange: () => void;
  isCLIMode: boolean;
};

const MigrationCheckbox = ({
  isChecked,
  onChange,
  isCLIMode,
}: MigrationCheckboxProps) =>
  isCLIMode && (
    <div className={`form-group ${styles.add_mar_top_small}`}>
      <label className={`col-sm-3 control-label ${styles.insertBoxLabel}`} />
      <label className={styles.labelText}>
        <input
          type="checkbox"
          checked={isChecked}
          title="This is a migration"
          onChange={onChange}
          className={`${styles.migrationCheckbox} legacy-input-fix`}
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
