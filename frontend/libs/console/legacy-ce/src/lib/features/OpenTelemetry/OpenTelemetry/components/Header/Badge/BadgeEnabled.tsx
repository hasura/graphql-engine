import * as React from 'react';
import { Badge } from '../../../../../../new-components/Badge';
import { FaCheckCircle } from 'react-icons/fa';

export function BadgeEnabled() {
  return (
    <Badge color="green" className="flex gap-2">
      <FaCheckCircle />
      Enabled
    </Badge>
  );
}
