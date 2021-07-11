import React from 'react';
// import styles from './KnowMoreLink.scss';

interface KnowMoreLinkProps {
  href: string,
  text?: string
}

const KnowMoreLink = ({ href, text = 'Know more' }: KnowMoreLinkProps) => (
  <a href={href} target="_blank" rel="noopener noreferrer">
    <small>
      <i>({text})</i>
    </small>
  </a>
);

export default KnowMoreLink;
