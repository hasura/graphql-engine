import React from 'react';

const Reload = ({ className = '' }) => {
  return <i className={`fa fa-repeat ${className || ''}`} aria-hidden="true" />;
};

export default Reload;
