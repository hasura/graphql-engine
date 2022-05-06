import React from 'react';
import { FaClock } from 'react-icons/fa';

const Clock = ({ className = '', title = '' }) => {
  return (
    <FaClock
      className={`${className || ''}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Clock;
