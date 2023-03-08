import * as React from 'react';

interface LearnMoreLinkProps {
  href: string;
  text?: string;
  className?: string;
}

/**
 * The updated LearnMoreLink component.
 * @see https://github.com/hasura/graphql-engine-mono/pull/7023
 */
export const LearnMoreLink: React.VFC<LearnMoreLinkProps> = props => {
  const { href, className = '', text = '(Learn More)' } = props;

  return (
    <a
      href={href}
      target="_blank"
      rel="noopener noreferrer"
      className={`ml-xs italic text-sm font-thin text-secondary ${className}`}
    >
      {text}
    </a>
  );
};
