import * as React from 'react';
import { Badge } from '../../../../../../new-components/Badge';
import { FaTimesCircle } from 'react-icons/fa';

export function BadgeDisabled() {
  return (
    <Badge color="gray" className="flex gap-2">
      <FaTimesCircle />
      Disabled
    </Badge>
  );
}
