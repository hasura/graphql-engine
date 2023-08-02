import React from 'react';
import { FaCheckCircle, FaTimesCircle } from 'react-icons/fa';
import { Badge } from '../../new-components/Badge';

type Props = {
  status: 'enabled' | 'disabled';
};

export function StatusBadge(props: Props) {
  const { status } = props;

  if (status === 'enabled')
    return (
      <Badge color="green" className="flex gap-2">
        <FaCheckCircle />
        Enabled
      </Badge>
    );

  return (
    <Badge color="gray" className="flex gap-2">
      <FaTimesCircle className="text-red-500" />
      Disabled
    </Badge>
  );
}
