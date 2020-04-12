import React from 'react';
import { css } from 'styled-components';

import { StyledTextLink } from './TextLink';

export const TextLink = props => {
  const { children, underline, type, href, hover, fontSize } = props;

  if (type === 'moreInfo') {
    return (
      <StyledTextLink
        {...props}
        href={href}
        fontSize={fontSize || '12px'}
        target='_blank'
        rel='noopener noreferrer'
        color='blue.link'
        fontStyle='italic'
        css={css`
          &:hover {
            text-decoration: underline !important;
          }
        `}
      >
        {`(${children || 'Know more'})`}
      </StyledTextLink>
    );
  }

  return (
    <StyledTextLink
      {...props}
      href={href}
      borderBottom={underline ? 1 : 'none'}
      rel='noopener noreferrer'
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
    </StyledTextLink>
  );
};

TextLink.defaultProps = {
  color: 'black.text',
  fontWeight: 'medium'
};
