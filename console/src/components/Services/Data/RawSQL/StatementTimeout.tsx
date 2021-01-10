import React, { FC } from 'react';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import styles from '../../../Common/TableCommon/Table.scss';
import globals from '../../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../../constants';

const StatementTimeout: FC<StatementTimeoutProps> = ({
  isMigrationChecked,
  statementTimeout,
  updateStatementTimeout,
}) => {
  return (
    <div className={styles.add_mar_top_small}>
      <label>
        Statement timeout (seconds)
        <ToolTip message="Abort statements that take longer than the specified time" />
        <input
          disabled={
            globals.consoleMode === CLI_CONSOLE_MODE && isMigrationChecked
          }
          title={
            isMigrationChecked
              ? 'Setting statement timeout is not supported for migrations'
              : ''
          }
          min={0}
          value={statementTimeout || ''}
          type="number"
          className={`${styles.inline_block} ${styles.tableNameInput} ${styles.add_mar_left} form-control`}
          data-test="raw-sql-statement-timeout"
          onChange={event => updateStatementTimeout(event.target.value)}
        />
      </label>
    </div>
  );
};

export default StatementTimeout;

interface StatementTimeoutProps {
  isMigrationChecked: boolean;
  statementTimeout: number;
  updateStatementTimeout: (e: string) => void;
}
