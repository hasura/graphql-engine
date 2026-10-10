import React from 'react';
import { Flex } from '@radix-ui/themes';
import { FieldLabel, Input } from '@hasura/shared/ui';
import { RetryConf } from '../../types';

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
    <Flex align="center" className="mb-2">
      <div className="w-64">
        <FieldLabel
          label={label}
          tooltip={
            tooltipProps?.message
              ? tooltipProps.message
              : 'Number of retries that Hasura makes to the webhook in case of failure'
          }
        />
      </div>
      <div>
        <Input type="number" min="0" {...inputProps} />
      </div>
    </Flex>
  );
};

const RetryConfEditor: React.FC<Props> = (props) => {
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
    <div className="mt-4">
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
    </div>
  );
};

export default RetryConfEditor;
