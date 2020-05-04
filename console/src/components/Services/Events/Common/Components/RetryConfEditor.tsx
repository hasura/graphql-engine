import React from 'react';
import { RetryConf } from '../../Types';
import styles from '../../Events.scss';

type Props = {
  setRetryConf: (r: RetryConf) => void;
  retryConf: RetryConf;
};

const RetryConfEditor = (props: Props) => {
  const { retryConf, setRetryConf } = props;

  const handleRetryConfChange = (e: React.BaseSyntheticEvent) => {
    const label = e.target.name;
    const value = e.target.value;
    setRetryConf({
      ...retryConf,
      [label]: parseInt(value, 10),
    });
  };

  return (
    <div className={styles.add_mar_top}>
      <h4 className={styles.subheading_text}>Retry Logic</h4>
      <div className={styles.retrySection}>
        <div className={`col-md-3 ${styles.padd_left_remove}`}>
          <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
            Number of retries (default: 0)
          </label>
        </div>
        <div className={`col-md-6 ${styles.padd_left_remove}`}>
          <input
            onChange={handleRetryConfChange}
            name="num_retries"
            data-test="no-of-retries"
            className={`${styles.display_inline} form-control ${styles.width300}`}
            type="number"
            min="0"
            value={retryConf.num_retries}
            placeholder="no of retries"
          />
        </div>
      </div>
      <div className={styles.retrySection}>
        <div className={`col-md-3 ${styles.padd_left_remove}`}>
          <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
            Retry Interval in seconds (default: 10)
          </label>
        </div>
        <div className={`col-md-6 ${styles.padd_left_remove}`}>
          <input
            onChange={handleRetryConfChange}
            name="interval_sec"
            data-test="interval-seconds"
            className={`${styles.display_inline} form-control ${styles.width300}`}
            type="number"
            min="0"
            value={retryConf.interval_sec}
            placeholder="interval time in seconds"
          />
        </div>
      </div>
      <div className={styles.retrySection}>
        <div className={`col-md-3 ${styles.padd_left_remove}`}>
          <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
            Timeout in seconds (default: 60)
          </label>
        </div>
        <div className={`col-md-6 ${styles.padd_left_remove}`}>
          <input
            onChange={handleRetryConfChange}
            name="timeout_sec"
            data-test="timeout-seconds"
            className={`${styles.display_inline} form-control ${styles.width300}`}
            type="number"
            min="0"
            value={retryConf.timeout_sec}
            placeholder="timeout in seconds"
          />
        </div>
      </div>
    </div>
  );
};

export default RetryConfEditor;
