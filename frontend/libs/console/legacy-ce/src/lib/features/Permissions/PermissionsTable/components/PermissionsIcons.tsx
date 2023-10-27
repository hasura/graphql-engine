import React from 'react';

import { FaCheck, FaExclamation, FaFilter, FaTimes } from 'react-icons/fa';

export interface PermissionsIconProps {
  type: 'fullAccess' | 'noAccess' | 'partialAccess' | 'partialAccessWarning';
  selected?: boolean;
}

export const PermissionsIcon = ({
  type,
  selected = false,
}: PermissionsIconProps) => {
  if (type === 'fullAccess') {
    return <FaCheck className={'text-green-600'} />;
  }

  if (type === 'partialAccess') {
    return (
      <FaFilter className={selected ? 'text-blue-900' : 'text-gray-600'} />
    );
  }

  if (type === 'partialAccessWarning') {
    return <FaExclamation />;
  }

  return <FaTimes className={'text-red-600'} />;
};
