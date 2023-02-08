import * as React from 'react';

interface KnowMoreLinkProps {
  href: string;
  text?: string;
  className?: string;
}

/**
 * The updated KnowMoreLink component.
 * @see https://github.com/hasura/graphql-engine-mono/pull/7023
 */
export const KnowMoreLink: React.VFC<KnowMoreLinkProps> = props => {
  const { href, className = '', text = '(Know More)' } = props;

  return (
    <a
      href={href}
      target="_blank"
      rel="noopener noreferrer"
      className={`ml-xs italic text-sm text-secondary ${className}`}
    >
      {text}
    </a>
  );
};
