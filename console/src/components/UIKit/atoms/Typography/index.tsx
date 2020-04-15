import React, { ReactNode } from 'react';

import { StyledHeading, StyledText, StyledTextLink } from './Typography';

export const Heading = StyledHeading;

Heading.defaultProps = {
  color: 'black.text',
};

interface TextProps {
  children?: ReactNode;
  type?: string;
  fontWeight?: string;
  fontSize?: string;
  as?: any;
  mb?: string;
  mt?: string;
  mr?: string;
  ml?: string;
  pl?: string;
}
/**
 * @example
 *  Explainer Text
 *  lineHeight: 'explain'
 *  fontSize: 'explain'
 *  fontWeight: 'bold'
 */
export const Text = (props: TextProps) => {
  const { children, type, fontWeight, fontSize } = props;

  const lineHeight = type === 'explain' ? 'body' : 'explain';

  let fontWeightValue;
  let fontSizeValue;

  if (fontWeight) {
    fontWeightValue = fontWeight;
  } else if (type === 'explain') {
    fontWeightValue = 'bold';
  }

  if (fontSize) {
    fontSizeValue = fontSize;
  } else {
    fontSizeValue = type === 'explain' ? 'explain' : 'p';
  }

  return (
    <StyledText
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSizeValue}
      fontWeight={fontWeightValue}
      color="black.text"
    >
      {children}
    </StyledText>
  );
};

Text.defaultProps = {
  mb: 'zero',
  mt: 'zero',
  mr: 'zero',
  ml: 'zero',
};

interface TextLinkProps {
  children: ReactNode;
  underline?: boolean;
  color?: string;
}
export const TextLink = (props: TextLinkProps) => {
  const { children, underline } = props;

  return (
    <StyledTextLink
      {...props}
      borderBottom={underline ? 2 : 'none'}
      borderColor={underline ? 'yellow.primary' : 'none'}
      fontWeight="medium"
      fontSize="p"
    >
      {children}
    </StyledTextLink>
  );
};

TextLink.defaultProps = {
  color: 'black.text',
};
