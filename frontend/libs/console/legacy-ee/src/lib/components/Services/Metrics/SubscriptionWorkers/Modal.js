import React from 'react';
import { Dialog } from '@hasura/console-legacy-ce';
import { Tooltip } from '@hasura/console-legacy-ce';
import CustomCopy from '../Common/CustomCopy';
import { REFETCH_DELAY, EXECUTION_TIME_DIVIDER_CONSTANT } from './constants';
import { hideVariableValues } from '../utils';

import styles from '../Metrics.module.scss';
import { formatRoundNumber } from '../../../../utils/math';
import inspectRow from '../images/warningNew.svg';

const Modal = props => {
  const { onHide, data } = props;

  const websocketData =
    data?.operation_logs?.length === 0 ? {} : data.operation_logs[0];

  const {
    poller_id: pollerId,
    time,
    last_execution_time: lastExecutionTime,
    refetch_delay: refetchDelay,
    batch_size: batchSize,
    user_role: userRole,
    execution_batch_size: executionBatchSize,
    total_subscribers: totalSubscribers,
    session_variables: sessionVariables,
    generated_sql: generatedSql,
  } = data;

  const {
    operation_id: operationId,
    operation_name: operationName,
    operation_string: operationString,
    variables,
  } = websocketData;

  const renderWarningSymbol = () => {
    if (
      lastExecutionTime * 0.001 >=
      REFETCH_DELAY / EXECUTION_TIME_DIVIDER_CONSTANT
    ) {
      return (
        <Tooltip
          side="right"
          tooltipContentChildren={
            'This subscription is taking as much time as the refetch delay to execute. ' +
            'You might want to adjust the batch size or adjust the delay to accomodate the execution time. ' +
            'You can also try optimising the SQL query by adding the right indexes.'
          }
        >
          <img
            className={styles.actionImg}
            src={inspectRow}
            alt={'Inspect row'}
          />
        </Tooltip>
      );
    }
    return null;
  };

  const renderGeneratedSql = () => {
    if (generatedSql) {
      try {
        return <CustomCopy label={'GENERATED SQL'} copy={generatedSql} />;
      } catch (e) {
        console.error(e);
      }
    }
    return null;
  };

  const renderOperationString = () => {
    if (operationString) {
      try {
        return <CustomCopy label={'OPERATION STRING'} copy={operationString} />;
      } catch (e) {
        console.error(e);
      }
    }
    return null;
  };

  const renderQueryVariables = () => {
    if (variables) {
      try {
        return (
          <CustomCopy
            label={'VARIABLES'}
            copy={JSON.stringify(hideVariableValues(variables), null, 2)}
          />
        );
      } catch (e) {
        console.error(e);
      }
    }
    return null;
  };

  const renderSessionVariables = () => {
    if (sessionVariables) {
      try {
        return (
          <CustomCopy
            label={'SESSION VARIABLES'}
            copy={JSON.stringify(hideVariableValues(sessionVariables), null, 2)}
          />
        );
      } catch (e) {
        console.error(e);
      }
    }
    return null;
  };

  return (
    <Dialog
      id="operationInspect"
      onClose={onHide}
      hasBackdrop
      size="xxl"
      title="Inspect"
    >
      <div className={styles.modalWrapper}>
        <div className={styles.modalContainer}>
          <div
            className={
              styles.noPadd +
              ' col-md-6 ' +
              styles.borderRight +
              ' ' +
              styles.flexColumn
            }
          >
            <div className={styles.infoWrapper}>
              <div className={`${styles.infoField} ${styles.paddingBottom}`}>
                <div className={styles.information}>
                  TIMESTAMP: <span>{new Date(time).toLocaleString()}</span>
                </div>
                <div
                  className={
                    styles.information +
                    ' ' +
                    styles.borderBottom +
                    ' ' +
                    styles.addPaddBottom
                  }
                >
                  WORKER ID: <span>{pollerId}</span>
                </div>
              </div>
              <div className={`${styles.infoField} ${styles.noPaddingBottom}`}>
                <div className={styles.information}>
                  LAST EXEC. TIME:
                  <span className={styles.paddingLeft}>
                    {`${formatRoundNumber(lastExecutionTime * 1000, 2)} ms`}
                  </span>
                  {renderWarningSymbol()}
                </div>
                <div className={styles.information}>
                  REFETCH DELAY: <span>{`${refetchDelay}s`}</span>
                </div>
                <div className={styles.information}>
                  NO. OF BATCHES: <span>{executionBatchSize}</span>
                </div>
                <div className={styles.information}>
                  EACH BATCH SIZE: <span>{batchSize}</span>
                </div>
                <div className={`${styles.information} ${styles.borderBottom}`}>
                  TOTAL SUBSCRIBERS: <span>{totalSubscribers}</span>
                </div>
              </div>
            </div>
            <div>{renderGeneratedSql()}</div>
          </div>
          <div className={`overflow-auto ${styles.noPadd} col-md-6`}>
            <div className={styles.infoWrapper}>
              <div className={styles.infoField} style={{ paddingBottom: 0 }}>
                <div className={styles.information}>
                  USER ROLE: <span>{userRole}</span>
                </div>
                <div className={styles.information}>
                  OPERATION ID: <span>{operationId}</span>
                </div>
                <div className={`${styles.information} ${styles.borderBottom}`}>
                  OPERATION NAME: <span>{operationName}</span>
                </div>
              </div>
            </div>
            {renderOperationString()}
            {renderQueryVariables()}
            {renderSessionVariables()}
          </div>
        </div>
      </div>
    </Dialog>
  );
};

export default Modal;
