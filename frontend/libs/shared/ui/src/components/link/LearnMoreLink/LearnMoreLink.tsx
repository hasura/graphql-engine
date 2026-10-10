import { Link } from '@radix-ui/themes';
import * as React from 'react';
import { LinkProps } from '../Link';

interface LearnMoreLinkProps extends Omit<LinkProps, 'children'> {
  href: string;
  text?: string;
}

/**
 * The updated LearnMoreLink component.
 */
export const LearnMoreLink: React.FC<LearnMoreLinkProps> = ({
  href,
  className = 'italic',
  text = '(Learn More)',
  target = '_blank',
  rel = 'noopener noreferrer',
  weight = 'light',
  size = '2',
  ...rest
}) => {
  return (
    <Link
      href={href}
      target={target}
      rel={rel}
      className={className}
      weight={weight}
      size={size}
      {...rest}
    >
      {text}
    </Link>
  );
};
