import React from 'react';

import { StyledHeading, StyledText, StyledTextLink } from './Typography';

export const Heading = props => {
  const { children, type } = props;

  if (type === 'subHeading') {
    return (
      <StyledHeading
        as="h4"
        fontSize="15px"
        pb="20px"
        mt="0px"
        mb="0px"
        {...props}
      >
        {children}
      </StyledHeading>
    );
  }
  // No else block here.

  return <StyledHeading {...props}>{children}</StyledHeading>;
};

Heading.defaultProps = {
  color: 'black.text',
  fontWeight: 'bold'
};

/**
 * @example
 *  Explainer Text
 *  lineHeight: 'explain'
 *  fontSize: 'explain'
 *  fontWeight: 'bold'
 */
export const Text = props => {
  const { children, type, fontWeight, fontSize } = props;

  const lineHeight = type === 'explain' ? 'explain' : 'body';

  const fontWeightValue = type === 'explain' ? 'bold' : '';

  const fontSizeValue = type === 'explain' ? 'explain' : '';

  return (
    <StyledText
      {...props}
      lineHeight={lineHeight}
      fontSize={fontSize || fontSizeValue}
      fontWeight={fontWeight || fontWeightValue}
    >
      {children}
    </StyledText>
  );
};

Text.defaultProps = {
  mb: 0,
  mt: 0
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
  color: 'black.text'
};
