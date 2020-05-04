import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { EventTrigger, RetryConf } from '../../Types';
import Tooltip from '../../../../Common/Tooltip/Tooltip';

type Callback = () => void;

type RetryConfEditorProps = {
  currentTrigger: EventTrigger;
  conf: RetryConf;
  setRetryConf: (r: RetryConf) => void;
  styles: Record<string, string>;
  save: (success: Callback, error: Callback) => void;
};

const RetryConfEditor = (props: RetryConfEditorProps) => {
  const { currentTrigger, conf, setRetryConf, styles, save } = props;

  const existingConf = currentTrigger.configuration.retry_conf;

  const reset = () => {
    setRetryConf(existingConf);
  };

  const handleChange = (e: React.BaseSyntheticEvent) => {
    const label = e.target.name;
    const value = e.target.value;
    console.log('Handling change');
    console.log(label);
    console.log(value);
    setRetryConf({
      ...conf,
      [label]: parseInt(value, 10),
    });
  };

  const collapsed = () => (
    <div className={styles.modifyOps}>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Number of retries:
        </div>
        <div className={`col-md-12 ${styles.padd_remove}`}>
          {existingConf.num_retries || 0}
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Retry Interval (sec):
        </div>
        <div className={`col-md-12 ${styles.padd_remove}`}>
          {existingConf.interval_sec || 10}
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>Timeout (sec):</div>
        <div className={`col-md-12 ${styles.padd_remove}`}>
          {existingConf.timeout_sec || 60}
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
            type="number"
            min="0"
            className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyRetryConfTextbox}`}
            value={conf.num_retries}
            name="num_retries"
            onChange={handleChange}
          />
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Retry interval (sec):&nbsp;
        </div>
        <div className="col-md-12">
          <input
            type="number"
            min="0"
            className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyRetryConfTextbox}`}
            value={conf.interval_sec}
            name="interval_sec"
            onChange={handleChange}
          />
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent1}>
        <div className={`col-md-4 ${styles.padd_remove}`}>
          Timeout (sec):&nbsp;
        </div>
        <div className="col-md-12">
          <input
            type="number"
            min="0"
            className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyRetryConfTextbox}`}
            value={conf.timeout_sec}
            name="timeout_sec"
            onChange={handleChange}
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
          property="retry"
          saveFunc={save}
          service="modify-trigger"
          expandCallback={reset}
          styles={styles}
        />
      </div>
    </div>
  );
};

export default RetryConfEditor;
