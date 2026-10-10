import * as React from 'react';
import { Badge } from '@hasura/shared/ui';
import { FaCheckCircle } from 'react-icons/fa';

export function BadgeEnabled() {
  return (
    <Badge color="green" className="flex gap-2">
      <FaCheckCircle />
      Enabled
    </Badge>
  );
}
