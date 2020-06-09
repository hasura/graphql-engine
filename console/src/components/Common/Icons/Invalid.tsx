import React from 'react';

const Invalid = ({ className = '', title = '' }) => {
  return (
    <i
      className={`fa fa-exclamation ${className || ''}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Invalid;
