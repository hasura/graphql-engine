import React from 'react';

interface KnowMoreLinkProps {
  href: string;
  text?: string;
}

const KnowMoreLink: React.VFC<KnowMoreLinkProps> = ({
  href,
  text = 'Know more',
}) => (
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
