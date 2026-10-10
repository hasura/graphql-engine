import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';

const ExclamationTriangle = ({ className = '', title = '' }) => {
  return (
    <FaExclamationTriangle
      className={className}
      aria-hidden="true"
      title={title}
    />
  );
};

export default ExclamationTriangle;
