import React from 'react';

const Skull = ({ className }) => {
  return <i className={`fa fa-skull ${className || ''}`} aria-hidden="true" />;
};

export default Skull;
