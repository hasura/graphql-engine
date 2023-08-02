import React from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { RetryConf } from '../../types';
import { inputStyles } from '../../constants';

type Props = {
  setRetryConf: (r: RetryConf) => void;
  retryConf: RetryConf;
  legacyTooltip?: boolean;
};

type RetryInputRowType = {
  label: string;
  tooltipProps?:
    | {
        message: string;
      }
    | undefined;
  inputProps: {
    name: string;
    'data-test': string;
    value: number | undefined;
    placeholder: string;
    onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  };
};

const RetryInputRow = ({
  label,
  tooltipProps,
  inputProps,
}: RetryInputRowType) => {
  return (
    <div className="items-center flex mb-xs">
      <div className="w-64 pl-0">
        <label className="flex items-center">
          {label}
          {tooltipProps ? (
            <IconTooltip {...tooltipProps} />
          ) : (
            <IconTooltip message="Number of retries that Hasura makes to the webhook in case of failure" />
          )}
        </label>
      </div>
      <div className="pl-0">
        <input
          className={`${inputStyles} w-64`}
          type="number"
          min="0"
          {...inputProps}
        />
      </div>
    </div>
  );
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
      <RetryInputRow
        label="Number of retries"
        inputProps={{
          name: 'num_retries',
          'data-test': 'no-of-retries',
          value: retryConf.num_retries,
          placeholder: 'number of retries (default: 0)',
          onChange: handleRetryConfChange,
        }}
        tooltipProps={{
          message:
            'Number of retries that Hasura makes to the webhook in case of failure',
        }}
      />
      <RetryInputRow
        label="Retry interval in seconds"
        inputProps={{
          name: 'interval_sec',
          'data-test': 'interval-seconds',
          value: retryConf.interval_sec,
          placeholder: 'interval time in seconds (default: 10)',
          onChange: handleRetryConfChange,
        }}
        tooltipProps={{
          message: 'Interval (in seconds) between each retry"',
        }}
      />

      <RetryInputRow
        label="Timeout in seconds"
        inputProps={{
          name: 'timeout_sec',
          'data-test': 'timeout-seconds',
          value: retryConf.timeout_sec,
          placeholder: 'timeout in seconds (default: 60)',
          onChange: handleRetryConfChange,
        }}
        tooltipProps={{
          message: 'Request timeout for the webhook',
        }}
      />

      {/* exists(retryConf.tolerance_sec) ? (
        <div
          className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}
        >
          <div className={`col-md-3 ${styles.padd_left_remove}`}>
            <label className={`${styles.add_mar_right} ${styles.retryLabel}`}>
              Tolerance in seconds
              <IconTooltip
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
