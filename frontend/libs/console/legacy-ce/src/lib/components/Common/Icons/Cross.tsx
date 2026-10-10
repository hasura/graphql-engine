import React from 'react';
import { FaTimes } from 'react-icons/fa';

const Cross = ({ className = '', title = '' }) => {
  return (
    <FaTimes
      className={`text-[#d9534f] text-xl ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Cross;
