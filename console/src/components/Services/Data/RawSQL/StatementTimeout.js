import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

const StatementTimeout = ({
  isMigrationChecked,
  statementTimeout,
  updateStatementTimeout,
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  const statementTimeoutTip = (
    <Tooltip id="tooltip-statement-timeout">
      Abort queries that take longer than the specified time
    </Tooltip>
  );

  const disabledStatementTimeoutTip = (
    <Tooltip id="tooltip-statement-timeout">
      Migrations cannot have a specified timeout
    </Tooltip>
  );

  return (
    <div className={styles.add_mar_top}>
      <label>
        Statement timeout(seconds)
        <OverlayTrigger
          placement="right"
          overlay={
            isMigrationChecked
              ? disabledStatementTimeoutTip
              : statementTimeoutTip
          }
        >
          <i
            className={`${styles.add_mar_left_small} fa fa-info-circle`}
            aria-hidden="true"
          />
        </OverlayTrigger>
        <input
          disabled={isMigrationChecked}
          min={0}
          value={statementTimeout || ''}
          type="number"
          className={`${styles.inline_block} ${styles.add_mar_left_small}`}
          data-test="raw-sql-statement-timeout"
          onChange={event => updateStatementTimeout(event.target.value)}
        />
      </label>
    </div>
  );
};

export default StatementTimeout;
