import React from 'react';
import { FaTimesCircle } from 'react-icons/fa';

const TimesCircle = ({ className = '', title = '' }) => {
  return (
    <FaTimesCircle className={className} aria-hidden="true" title={title} />
  );
};

export default TimesCircle;
