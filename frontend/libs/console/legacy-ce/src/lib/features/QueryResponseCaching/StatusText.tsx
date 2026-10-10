import React from 'react';
import { FaCheckCircle, FaRegCircle } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

type Props = {
  status: 'enabled' | 'disabled';
};

export function StatusText(props: Props) {
  const { status } = props;

  return (
    <div className="text-muted">
      <div className="font-semibold">Current Status</div>
      {status === 'enabled' ? (
        <Flex align="center">
          <FaCheckCircle className="mr-1 text-emerald-600" />
          Cache Enabled
        </Flex>
      ) : (
        <Flex align="center">
          <FaRegCircle className="mr-1" />
          Cache Disabled
        </Flex>
      )}
    </div>
  );
}
