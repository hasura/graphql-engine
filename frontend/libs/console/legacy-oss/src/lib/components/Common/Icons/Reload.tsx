import React from 'react';
import { FaRedoAlt } from 'react-icons/fa';

const Reload = ({ className = '' }) => {
  return <FaRedoAlt className={` ${className || ''}`} aria-hidden="true" />;
};

export default Reload;
