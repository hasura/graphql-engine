import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import globals from '../../../../Globals';

const isCLIMode = globals.consoleMode === CLI_CONSOLE_MODE;

type MigrationCheckboxProps = {
  isChecked: boolean;
  onChange: () => void;
  className: string;
};

const MigrationCheckbox: React.FC<MigrationCheckboxProps> = ({
  isChecked,
  onChange,
  className,
}) => {
  if(isCLIMode===true) {
    return (
      <div className={className}>
        <label className={styles.labelText}>
          <input
            type="checkbox"
            checked={isChecked}
            title="This is a migration"
            onChange={onChange}
            className={styles.migrationCheckbox} />
          This is a migration
          <ToolTip
            placement="right"
            message="Create a migration file with the current insertion" />
        </label>
      </div>
    )
  }
  else {
    return (null)
  }
};
console.log(isCLIMode);
console.log('Varun___________________________________________!23');

export default MigrationCheckbox;
