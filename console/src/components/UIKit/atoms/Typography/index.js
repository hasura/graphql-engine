import React from 'react';

import { StyledHeading, StyledText, StyledTextLink } from './Typography';

export const Heading = props => {
  const { children } = props;

  return <StyledHeading {...props}>{children}</StyledHeading>;
};

Heading.defaultProps = {
  color: 'black.text',
};

export const Text = props => {
  const { children, type, fontWeight, fontSize } = props;

  /* Explainer Text
   *  lineHeight: 'explain'
   *  fontSize: 'explain'
   *  fontWeight: 'bold'
   */

  const lineHeight = type === 'explain' ? 'body' : 'explain';

  // fontWeight & fontSize value can be received as prop to override default styles.

  let fontWeightValue;
  let fontSizeValue;

  if (fontWeight) {
    fontWeightValue = fontWeight;
  } else if (type === 'explain') {
    fontWeightValue = 'bold';
  }
  // No else clause here.

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

export const TextLink = props => {
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
