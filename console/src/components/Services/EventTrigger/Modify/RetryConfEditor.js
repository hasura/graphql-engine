import React from 'react';
import Editor from './Editor';

import { setRetryNum, setRetryInterval, setRetryTimeout } from './Actions';
import Tooltip from './Tooltip';

import semverCheck from '../../../../helpers/semver';

class RetryConfEditor extends React.Component {
  state = {
    supportRetryTimeout: false,
  };

  componentDidMount() {
    if (this.props.serverVersion) {
      this.checkRetryTimeoutSupport(this.props.serverVersion);
    }
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.serverVersion !== this.props.serverVersion) {
      this.checkRetryTimeoutSupport(this.props.serverVersion);
    }
  }

  setValues = () => {
    const { dispatch } = this.props;
    const retryConf = this.props.retryConf || {};
    dispatch(setRetryNum(retryConf.num_retries || 0));
    dispatch(setRetryInterval(retryConf.interval_sec || 10));
    if (this.state.supportRetryTimeout) {
      dispatch(setRetryTimeout(retryConf.timeout_sec || 60));
    }
  };

  checkRetryTimeoutSupport(version) {
    const supportRetryTimeout = semverCheck('triggerRetryTimeout', version);
    this.setState({ supportRetryTimeout });
  }

  validateAndSave = () => {
    const {
      dispatch,
      modifyTrigger: {
        retryConf: { numRetrys, retryInterval, timeout },
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

    if (this.state.supportRetryTimeout) {
      if (isNaN(retryInterval)) {
        alert('Retry interval should be an integer!');
        return;
      }
      dispatch(setRetryTimeout(parseInt(timeout, 10)));
    }

    this.props.save();
  };

  render() {
    const { styles, dispatch, modifyTrigger } = this.props;
    const retryConf = this.props.retryConf || {};
    const { supportRetryTimeout } = this.state;
    const collapsed = toggleButton => (
      <div className={styles.modifyOpsCollapsed}>
        {toggleButton('Edit')}
        <div className={styles.modifyOps}>
          <div className={styles.modifyOpsCollapsedContent1}>
            <div className={'col-md-4 ' + styles.noPadd}>
              Number of retries:
            </div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {retryConf.num_retries}
            </div>
          </div>
          <div className={styles.modifyOpsCollapsedContent1}>
            <div className={'col-md-4 ' + styles.noPadd}>
              Retry Interval (sec):
            </div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {retryConf.interval_sec}
            </div>
          </div>
          {supportRetryTimeout && (
            <div className={styles.modifyOpsCollapsedContent1}>
              <div className={'col-md-4 ' + styles.noPadd}>Timeout (sec):</div>
              <div className={'col-md-12 ' + styles.noPadd}>
                {retryConf.timeout_sec}
              </div>
            </div>
          )}
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
          {this.state.supportRetryTimeout && (
            <div className={styles.modifyOpsCollapsedContent1}>
              <div className={`col-md-4 ${styles.noPadd}`}>
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
          )}
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
