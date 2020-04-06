import React from 'react';

const ExternalLink = ({ className }) => {
  return (
    <i
      className={`fa fa-external-link ${className || ''}`}
      aria-hidden="true"
    />
  );
};

export default ExternalLink;
