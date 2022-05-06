import React from 'react';
import { FaCopy } from 'react-icons/fa';

const Copy = ({ className = '' }) => {
  return <FaCopy className={`${className || ''}`} aria-hidden="true" />;
};

export default Copy;
