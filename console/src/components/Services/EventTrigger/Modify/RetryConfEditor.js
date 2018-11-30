import React from 'react';
import Editor from './Editor';

import { setRetryNum, setRetryInterval } from './Actions';

class RetryConfEditor extends React.Component {
  setValues = () => {
    const { dispatch } = this.props;
    const retryConf = this.props.retryConf || {};
    dispatch(setRetryNum(retryConf.num_retries || 0));
    dispatch(setRetryInterval(retryConf.interval_sec || 10));
  };

  render() {
    const { styles, save, dispatch, modifyTrigger } = this.props;
    const retryConf = this.props.retryConf || {};
    const collapsed = toggleButton => (
      <div className={styles.modifyOpsCollapsed}>
        {toggleButton('Edit')}
        <div className={styles.modifyOps}>
          <div className={styles.modifyOpsCollapsedContent}>
            Number of retries: {retryConf.num_retries || 0}
          </div>
          <div className={styles.modifyOpsCollapsedContent}>
            Retry interval (seconds): {retryConf.interval_sec || 10}
          </div>
        </div>
      </div>
    );

    const expanded = (toggleButton, saveButton) => (
      <div className={styles.modifyOpsExpanded}>
        {toggleButton('Close')}
        <div className={styles.modifyOpsPadLeft}>
          <div className={styles.modifyOpsCollapsedContent}>
            Number of retries: &nbsp;
            <input
              type="text"
              value={modifyTrigger.retryConf.numRetrys}
              className={`${styles.input} form-control ${
                styles.add_mar_right
              } ${styles.modifyRetryConfTextbox}`}
              onChange={e => dispatch(setRetryNum(e.target.value))}
            />
          </div>
          <div className={styles.modifyOpsCollapsedContent}>
            Retry interval (seconds):&nbsp;
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
        {saveButton(save)}
      </div>
    );

    return (
      <div className={styles.container}>
        <div className={styles.modifySection}>
          <h4 className={styles.modifySectionHeading}>Retry configuration</h4>
          <Editor
            editorCollapsed={collapsed}
            editorExpanded={expanded}
            toggleCallback={this.setValues}
            styles={styles}
          />
        </div>
      </div>
    );
  }
}

export default RetryConfEditor;
