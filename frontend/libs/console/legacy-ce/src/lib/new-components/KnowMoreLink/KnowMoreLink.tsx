import * as React from 'react';

interface KnowMoreLinkProps {
  href: string;
}

/**
 * The updated KnowMoreLink component.
 * @see https://github.com/hasura/graphql-engine-mono/pull/7023
 */
export const KnowMoreLink: React.VFC<KnowMoreLinkProps> = props => {
  const { href } = props;

  return (
    <a
      href={href}
      target="_blank"
      rel="noopener noreferrer"
      className="ml-xs italic text-sm text-secondary"
    >
      (Know More)
    </a>
  );
};
