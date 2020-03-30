import React from 'react';

import Editor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  setRetryNum,
  setRetryInterval,
  setRetryTimeout,
  showValidationError,
} from './Actions';
import { ToolTip, Heading } from '../../../UIKit/atoms';

const RetryConfEditor = props => {
  const { dispatch, save, styles, modifyTrigger } = props;
  const retryConf = props.retryConf || {};

  const setValues = () => {
    dispatch(setRetryNum(retryConf.num_retries || 0));
    dispatch(setRetryInterval(retryConf.interval_sec || 10));
    dispatch(setRetryTimeout(retryConf.timeout_sec || 60));
  };

  const validateAndSave = () => {
    const {
      retryConf: { numRetrys, retryInterval, timeout },
    } = modifyTrigger;

    const iNumRetries = numRetrys === '' ? 0 : parseInt(numRetrys, 10);
    const iRetryInterval =
      retryInterval === '' ? 10 : parseInt(retryInterval, 10);
    const iTimeout = timeout === '' ? 60 : parseInt(timeout, 10);

    if (iNumRetries < 0 || isNaN(iNumRetries)) {
      dispatch(
        showValidationError('Number of retries must be a non negative number!')
      );
      return;
    }

    dispatch(setRetryNum(iNumRetries));

    if (iRetryInterval <= 0 || isNaN(iRetryInterval)) {
      dispatch(showValidationError('Retry interval must be a postive number!'));
      return;
    }

    dispatch(setRetryInterval(iRetryInterval));

    if (isNaN(iTimeout) || iTimeout <= 0) {
      dispatch(showValidationError('Timeout must be a positive number!'));
      return;
    }

    dispatch(setRetryTimeout(iTimeout));

    save();
  };

  const collapsed = () => (
    <div className={styles.modifyOps}>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={'col-md-4 ' + styles.padd_remove}>
          Number of retries:
        </div>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {retryConf.num_retries || 0}
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={'col-md-4 ' + styles.padd_remove}>
          Retry Interval (sec):
        </div>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {retryConf.interval_sec || 10}
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={'col-md-4 ' + styles.padd_remove}>Timeout (sec):</div>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {retryConf.timeout_sec || 60}
        </div>
      </div>
    </div>
  );

  const expanded = () => (
    <div className={styles.modifyOpsPadLeft}>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Number of retries: &nbsp;
        </div>
        <div className="col-md-12">
          <input
            type="text"
            value={modifyTrigger.retryConf.numRetrys}
            className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyRetryConfTextbox}`}
            onChange={e => dispatch(setRetryNum(e.target.value))}
          />
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Retry interval (sec):&nbsp;
        </div>
        <div className="col-md-12">
          <input
            type="text"
            className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyRetryConfTextbox}`}
            value={modifyTrigger.retryConf.retryInterval}
            onChange={e => dispatch(setRetryInterval(e.target.value))}
          />
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Timeout (sec):&nbsp;
        </div>
        <div className="col-md-12">
          <input
            type="text"
            className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyRetryConfTextbox}`}
            value={modifyTrigger.retryConf.timeout}
            onChange={e => dispatch(setRetryTimeout(e.target.value))}
          />
        </div>
      </div>
    </div>
  );

  return (
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <Heading as="h4" fontSize="15px" mb="20px">
          Retry configuration
          <ToolTip
            message="Edit your retry settings for event failures"
            ml="sm"
          />
        </Heading>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          ongoingRequest={modifyTrigger.ongoingRequest}
          property={'retry'}
          saveFunc={validateAndSave}
          service="modify-trigger"
          expandCallback={setValues}
          styles={styles}
        />
      </div>
    </div>
  );
};

export default RetryConfEditor;
