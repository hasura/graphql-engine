import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { EventTrigger, RetryConf, VoidCallback } from '../../types';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import CommonRetryConf from '../../Common/Components/RetryConfEditor';

type RetryConfEditorProps = {
  currentTrigger: EventTrigger;
  conf: RetryConf;
  setRetryConf: (r: RetryConf) => void;
  styles: Record<string, string>;
  save: (success: VoidCallback, error: VoidCallback) => void;
};

const RetryConfEditor = (props: RetryConfEditorProps) => {
  const { currentTrigger, conf, setRetryConf, styles, save } = props;

  const existingConf = currentTrigger.configuration.retry_conf;

  const reset = () => {
    setRetryConf(existingConf);
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
    <CommonRetryConf retryConf={conf} setRetryConf={setRetryConf} />
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
