import React from 'react';

const Download = ({ className }) => {
  return (
    <i className={`fa fa-download ${className || ''}`} aria-hidden="true" />
  );
};

export default Download;
