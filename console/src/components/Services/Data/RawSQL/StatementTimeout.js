import React from 'react';
import ToolTip from '../../../Common/Tooltip/Tooltip';

const StatementTimeout = ({
  isMigrationChecked,
  statementTimeout,
  updateStatementTimeout,
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');

  return (
    <div className={styles.add_mar_top_small}>
      <label>
        Statement timeout (seconds)
        <ToolTip
          message={'Abort statements that take longer than the specified time on Postgres'}
        />
        <input
          disabled={isMigrationChecked}
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
