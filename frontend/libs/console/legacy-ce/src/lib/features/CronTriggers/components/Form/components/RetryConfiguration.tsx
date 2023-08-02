import React from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { useFormContext } from 'react-hook-form';

const inputStyes =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';

export const RetryConfiguration = () => {
  const { register } = useFormContext();

  return (
    <div className="space-y-sm relative max-w-xl">
      <div className="grid grid-cols-12 gap-3">
        <div className="col-span-6 flex items-center">
          <label className="block">Number of retries</label>
          <IconTooltip message="Number of retries that Hasura makes to the webhook in case of failure" />
        </div>
        <div className="col-span-6">
          {/* TODO: This is a horizontal/inline input field, currently we do not have it in common so this component implements its own,
                 we should replace this in future with the common component */}
          <input
            type="number"
            className={inputStyes}
            aria-label="num_retries"
            {...register('num_retries')}
          />
        </div>
      </div>
      <div className="grid grid-cols-12 gap-3">
        <div className="col-span-6 flex items-center">
          <label className="block">Retry interval in seconds</label>
          <IconTooltip message="Interval (in seconds) between each retry" />
        </div>
        <div className="col-span-6">
          <input
            type="number"
            className={inputStyes}
            aria-label="retry_interval_seconds"
            {...register('retry_interval_seconds')}
          />
        </div>
      </div>
      <div className="grid grid-cols-12 gap-3">
        <div className="col-span-6 flex items-center">
          <label className="block">Timeout in seconds</label>
          <IconTooltip message="Request timeout for the webhook" />
        </div>
        <div className="col-span-6">
          <input
            type="number"
            className={inputStyes}
            aria-label="timeout_seconds"
            {...register('timeout_seconds')}
          />
        </div>
      </div>
      <div className="grid grid-cols-12 gap-3">
        <div className="col-span-6 flex items-center">
          <label className="block">Tolerance in seconds</label>
          <IconTooltip message="Number of seconds between scheduled time and actual delivery time that is acceptable" />
        </div>
        <div className="col-span-6">
          <input
            type="number"
            className={inputStyes}
            aria-label="tolerance_seconds"
            {...register('tolerance_seconds')}
          />
        </div>
      </div>
    </div>
  );
};
