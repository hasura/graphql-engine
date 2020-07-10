import React from 'react';
import globals from '../../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import styles from '../../../Common/TableCommon/Table.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';

type MigrationCheckBoxProps = {
  isChecked: boolean;
  onChange: () => void;
};

const MigrationCheckBox: React.FC<MigrationCheckBoxProps> = ({
  isChecked,
  onChange,
}) => {
  const isCLIMode = globals.consoleMode === CLI_CONSOLE_MODE;
  return (
    <>
      {isCLIMode && (
        <label className={styles.labelText}>
          <input
            type="checkbox"
            checked={isChecked}
            title="This is a migration"
            onChange={onChange}
            className={styles.migrationCheckbox}
          />
          This is a migration
          <ToolTip placement="right" message="creates an insert migration" />
        </label>
      )}
    </>
  );
};

export default MigrationCheckBox;
