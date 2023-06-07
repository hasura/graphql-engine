import React from 'react';
import { FaCheckCircle, FaRegCircle } from 'react-icons/fa';

type Props = {
  status: 'enabled' | 'disabled';
};

export function StatusText(props: Props) {
  const { status } = props;

  return (
    <div className="text-muted">
      <div className="font-semibold">Current Status</div>
      {status === 'enabled' ? (
        <div className="flex items-center">
          <FaCheckCircle className="mr-xs text-emerald-600" />
          Cache Enabled
        </div>
      ) : (
        <div className="flex items-center">
          <FaRegCircle className="mr-xs" />
          Cache Disabled
        </div>
      )}
    </div>
  );
}
