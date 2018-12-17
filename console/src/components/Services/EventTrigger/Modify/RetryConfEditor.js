import React from 'react';
import Editor from './Editor';

import { setRetryNum, setRetryInterval } from './Actions';
import Tooltip from './Tooltip';

class RetryConfEditor extends React.Component {
  setValues = () => {
    const { dispatch } = this.props;
    const retryConf = this.props.retryConf || {};
    dispatch(setRetryNum(retryConf.num_retries || 0));
    dispatch(setRetryInterval(retryConf.interval_sec || 10));
  };

  validateAndSave = () => {
    const {
      dispatch,
      modifyTrigger: {
        retryConf: { numRetrys, retryInterval },
      },
    } = this.props;
    if (isNaN(numRetrys)) {
      alert('Number of retries should be an integer!');
      return;
    }
    dispatch(setRetryNum(parseInt(numRetrys, 10)));

    if (isNaN(retryInterval)) {
      alert('Retry interval should be an integer!');
      return;
    }
    dispatch(setRetryInterval(parseInt(retryInterval, 10)));

    this.props.save();
  };

  render() {
    const { styles, dispatch, modifyTrigger } = this.props;
    const retryConf = this.props.retryConf || {};
    const collapsed = toggleButton => (
      <div className={styles.modifyOpsCollapsed}>
        {toggleButton('Edit')}
        <div className={styles.modifyOps}>
          <div className={styles.modifyOpsCollapsedContent1}>
            <div className={'col-md-4 ' + styles.noPadd}>
              Number of retries:
            </div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {retryConf.num_retries || 0}
            </div>
          </div>
          <div className={styles.modifyOpsCollapsedContent1}>
            <div className={'col-md-4 ' + styles.noPadd}>
              Retry Interval (sec):
            </div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {retryConf.interval_sec || 0}
            </div>
          </div>
        </div>
      </div>
    );

    const expanded = (toggleButton, saveButton) => (
      <div className={styles.modifyOpsExpanded}>
        {toggleButton('Close')}
        <div className={styles.modifyOpsPadLeft}>
          <div className={styles.modifyOpsCollapsedContent1}>
            <div className={`col-md-4 ${styles.noPadd}`}>
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
            <div className={`col-md-4 ${styles.noPadd}`}>
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
        </div>
        {saveButton(this.validateAndSave)}
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
            toggleCallback={this.setValues}
            styles={styles}
          />
        </div>
      </div>
    );
  }
}

export default RetryConfEditor;
