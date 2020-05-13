import React from 'react';

const Clock = ({ className }) => {
  return (
    <i className={`fa fa-clock-o ${className || ''}`} aria-hidden="true" />
  );
};

export default Clock;
