import React from 'react';
import { FaExclamation } from 'react-icons/fa';

const Invalid = ({ className = '', title = '' }) => {
  return (
    <FaExclamation
      className={`${className || ''}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Invalid;
