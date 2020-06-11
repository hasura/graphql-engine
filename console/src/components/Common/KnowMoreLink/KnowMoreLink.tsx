import React from 'react';
// import styles from './KnowMoreLink.scss';

type KnowMoreLinkProps = {
  href: string;
  text?: string;
};

const KnowMoreLink: React.FC<KnowMoreLinkProps> = ({
  href,
  text = 'Know more',
}) => (
  <a href={href} target="_blank" rel="noopener noreferrer">
    <small>
      <i>({text})</i>
    </small>
  </a>
);

export default KnowMoreLink;
