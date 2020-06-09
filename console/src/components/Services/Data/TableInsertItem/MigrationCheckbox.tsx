import React from 'react';
import globals from '../../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import styles from '../../../Common/TableCommon/Table.scss';

type MigrationCheckBoxProps = {
  isChecked: boolean;
  onChange: () => void;
};

const MigrationCheckBox: React.FC<MigrationCheckBoxProps> = ({
  isChecked,
  onChange,
}) => {
  const isConsoleMode = globals.consoleMode === CLI_CONSOLE_MODE;
  return (
    <>
      {isConsoleMode && (
        <label className={styles.labelText}>
          <input
            type="checkbox"
            checked={isChecked}
            title="This is a Migration"
            onChange={onChange}
            className={styles.migrationCheckbox}
          />
          This is a Migration
        </label>
      )}
    </>
  );
};

export default MigrationCheckBox;
