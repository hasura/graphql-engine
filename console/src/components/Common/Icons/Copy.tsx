import React from 'react';

const Copy = ({ className = '' }) => {
  return <i className={`fa fa-copy ${className || ''}`} aria-hidden="true" />;
};

export default Copy;
