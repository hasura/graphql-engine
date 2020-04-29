import React from 'react';
import { css } from 'styled-components';

import { StyledLink } from './Link';

export const Link = props => {
  const { children, underline, type, href, hover, fontSize } = props;

  if (type === 'moreInfo') {
    return (
      <StyledLink
        {...props}
        href={href}
        fontSize={fontSize || '12px'}
        target="_blank"
        rel="noopener noreferrer"
        fontStyle="italic"
        css={css`
          &:hover {
            text-decoration: underline !important;
          }
        `}
      >
        {`(${children || 'Know more'})`}
      </StyledLink>
    );
  }

  return (
    <StyledLink
      {...props}
      href={href}
      borderBottom={underline ? 1 : 'none'}
      rel="noopener noreferrer"
      fontSize={fontSize || '14px'}
      css={
        hover === 'underline' &&
        css`
          &:hover {
            text-decoration: underline !important;
          }
        `
      }
    >
      {children}
    </StyledLink>
  );
};

Link.defaultProps = {
  color: 'blue.link',
  fontWeight: 'medium'
};
