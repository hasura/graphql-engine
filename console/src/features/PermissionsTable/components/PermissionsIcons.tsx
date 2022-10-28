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
  const selectedTextColour = 'text-white';
  if (type === 'fullAccess') {
    return (
      <FaCheck className={selected ? selectedTextColour : 'text-green-600'} />
    );
  }

  if (type === 'partialAccess') {
    return (
      <FaFilter className={selected ? selectedTextColour : 'text-gray-400'} />
    );
  }

  if (type === 'partialAccessWarning') {
    return <FaExclamation />;
  }

  return <FaTimes className={selected ? selectedTextColour : 'text-red-600'} />;
};
