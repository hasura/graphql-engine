import React from 'react';
import Editor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  setRetryNum,
  setRetryInterval,
  setRetryTimeout,
  showValidationError,
} from './Actions';
import Tooltip from '../../../Common/Tooltip/Tooltip';

class RetryConfEditor extends React.Component {
  setValues = () => {
    const { dispatch } = this.props;
    const retryConf = this.props.retryConf || {};
    dispatch(setRetryNum(retryConf.num_retries || 0));
    dispatch(setRetryInterval(retryConf.interval_sec || 10));
    dispatch(setRetryTimeout(retryConf.timeout_sec || 60));
  };

  validateAndSave = () => {
    const {
      dispatch,
      modifyTrigger: {
        retryConf: { numRetrys, retryInterval, timeout },
      },
    } = this.props;

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

    this.props.save();
  };

  render() {
    const { styles, dispatch, modifyTrigger } = this.props;
    const retryConf = this.props.retryConf || {};

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
              className={`${styles.input} form-control ${
                styles.add_mar_right
              } ${styles.modifyRetryConfTextbox}`}
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
              className={`${styles.input} form-control ${
                styles.add_mar_right
              } ${styles.modifyRetryConfTextbox}`}
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
              className={`${styles.input} form-control ${
                styles.add_mar_right
              } ${styles.modifyRetryConfTextbox}`}
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
          <h4 className={styles.modifySectionHeading}>
            Retry configuration{' '}
            <Tooltip message="Edit your retry settings for event failures" />
          </h4>
          <Editor
            editorCollapsed={collapsed}
            editorExpanded={expanded}
            ongoingRequest={modifyTrigger.ongoingRequest}
            property={'retry'}
            saveFunc={this.validateAndSave}
            service="modify-trigger"
            expandCallback={this.setValues}
            styles={styles}
          />
        </div>
      </div>
    );
  }
}

export default RetryConfEditor;
