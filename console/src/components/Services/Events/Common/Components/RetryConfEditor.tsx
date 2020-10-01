import React from 'react';
import { RetryConf } from '../../types';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import styles from '../../Events.scss';

type Props = {
  setRetryConf: (r: RetryConf) => void;
  retryConf: RetryConf;
};

const RetryConfEditor: React.FC<Props> = props => {
  const { retryConf, setRetryConf } = props;

  const handleRetryConfChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const label = e.target.name;
    const value = e.target.value;
    setRetryConf({
      ...retryConf,
      [label]: parseInt(value, 10),
    });
  };

  return (
    <div>
      <div className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}>
        <div className={`col-md-3 ${styles.padd_left_remove}`}>
          <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
            Number of retries
            <Tooltip
              id="retry-conf-num-retries"
              message="Number of retries that Hasura makes to the webhook in case of failure"
            />
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
            placeholder="number of retries (default: 0)"
          />
        </div>
      </div>
      <div className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}>
        <div className={`col-md-3 ${styles.padd_left_remove}`}>
          <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
            Retry interval in seconds
            <Tooltip
              id="retry-conf-interval-sec"
              message="Interval (in seconds) between each retry"
            />
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
            placeholder="interval time in seconds (default: 10)"
          />
        </div>
      </div>
      <div className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}>
        <div className={`col-md-3 ${styles.padd_left_remove}`}>
          <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
            Timeout in seconds
            <Tooltip
              id="retry-conf-timeout-sec"
              message="Request timeout for the webhook"
            />
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
            placeholder="timeout in seconds (default: 60)"
          />
        </div>
      </div>
      {/* exists(retryConf.tolerance_sec) ? (
        <div
          className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}
        >
          <div className={`col-md-3 ${styles.padd_left_remove}`}>
            <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
              Tolerance in seconds
              <Tooltip
                id="retry-conf-interval-sec"
                message="If scheduled time for an event is in the past, it gets dropped if it is older than the tolerance limit"
              />
            </label>
          </div>
           <div className={`col-md-6 ${styles.padd_left_remove}`}>
            <input
              onChange={handleRetryConfChange}
              name="tolerance_sec"
              data-test="tolerance-seconds"
              className={`${styles.display_inline} form-control ${styles.width300}`}
              type="number"
              min="0"
              value={retryConf.tolerance_sec || ''}
              placeholder="tolerance in seconds (default: 21600)"
            />
          </div>
        </div>
      ) : null TODO */}
    </div>
  );
};

export default RetryConfEditor;
