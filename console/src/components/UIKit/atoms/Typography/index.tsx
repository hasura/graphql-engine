import React from 'react';

import { StyledHeading, StyledText, StyledTextLink } from './Typography';
import { Theme } from '../../theme';

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
  fontWeight: 'bold',
};

/**
 * @example
 *  Explainer Text
 *  lineHeight: 'explain'
 *  fontSize: 'explain'
 *  fontWeight: 'bold'
 */
export type TextProps = {
  type: keyof Theme['lineHeights'];
  fontWeight: keyof Theme['fontWeights'];
  fontSize: keyof Theme['fontSizes'];
  mb: keyof Theme['space'];
  mt: keyof Theme['space'];
  mr: keyof Theme['space'];
  ml: keyof Theme['space'];
};

export const Text: React.FC<TextProps> = props => {
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

type TextLinkProps = {
  underline: boolean;
  color: string;
};

export const TextLink: React.FC<TextLinkProps> = props => {
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
