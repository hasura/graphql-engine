import React from 'react';

const Clock = ({ className = '', title = '' }) => {
  return (
    <i
      className={`fa fa-clock-o ${className || ''}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Clock;
