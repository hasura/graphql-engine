import React from 'react';

const KnowMoreLink = ({ href, text = 'Know more' }) => (
  <a
    href={href}
    target="_blank"
    className="ml-sm font-normal text-secondary text-italic text-sm"
    rel="noopener noreferrer"
  >
    ({text})
  </a>
);

export default KnowMoreLink;
