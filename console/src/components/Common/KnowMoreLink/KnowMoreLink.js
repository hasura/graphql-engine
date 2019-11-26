import React from 'react';
// import styles from './KnowMoreLink.scss';

const KnowMoreLink = ({ href }) => (
  <a href={href} target="_blank" rel="noopener noreferrer">
    <small>
      <i>(Know more)</i>
    </small>
  </a>
);

export default KnowMoreLink;
